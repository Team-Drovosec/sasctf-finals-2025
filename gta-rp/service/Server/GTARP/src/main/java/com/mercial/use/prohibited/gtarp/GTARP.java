package com.mercial.use.prohibited.gtarp;

import com.destroystokyo.paper.event.entity.PreCreatureSpawnEvent;
import com.sk89q.worldedit.bukkit.BukkitAdapter;
import com.sk89q.worldedit.math.BlockVector2;
import com.sk89q.worldedit.math.BlockVector3;
import com.sk89q.worldguard.WorldGuard;
import com.sk89q.worldguard.bukkit.BukkitPlayer;
import com.sk89q.worldguard.bukkit.WorldGuardPlugin;
import com.sk89q.worldguard.domains.DefaultDomain;
import com.sk89q.worldguard.protection.flags.Flags;
import com.sk89q.worldguard.protection.flags.StateFlag;
import com.sk89q.worldguard.protection.managers.RegionManager;
import com.sk89q.worldguard.protection.regions.ProtectedCuboidRegion;
import com.sk89q.worldguard.protection.regions.ProtectedRegion;
import io.r2dbc.spi.*;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;
import org.bukkit.*;
import org.bukkit.block.Block;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.conversations.ConversationContext;
import org.bukkit.conversations.ConversationFactory;
import org.bukkit.conversations.Prompt;
import org.bukkit.conversations.StringPrompt;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.generator.ChunkGenerator;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.SkullMeta;
import org.bukkit.persistence.PersistentDataType;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;
import io.r2dbc.pool.ConnectionPool;
import io.r2dbc.pool.ConnectionPoolConfiguration;
import io.r2dbc.postgresql.PostgresqlConnectionConfiguration;
import io.r2dbc.postgresql.PostgresqlConnectionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

public final class GTARP extends JavaPlugin implements Listener {

    public static class GeneratorUtils {
        public record GridCoordinates(int x, int z) {
        }

        public static GridCoordinates calculateDiagonalGridPosition(int buildingIndex) {
            int value = Math.max(0, buildingIndex);
            int diagonal = 0;
            long diagonalSum = 0;
            while (diagonalSum + diagonal + 1L <= value) {
                diagonal += 1;
                diagonalSum += diagonal;
            }
            int offset = (int) (value - diagonalSum);
            int gridXIndex;
            int gridZIndex;
            if ((diagonal & 1) == 0) {
                gridXIndex = offset;
                gridZIndex = diagonal - offset;
            } else {
                gridXIndex = diagonal - offset;
                gridZIndex = offset;
            }
            return new GridCoordinates(gridXIndex, gridZIndex);
        }

        public static void ensureChunksLoaded(World world, int minX, int maxX, int minZ, int maxZ) {
            for (int chunkX = minX >> 4; chunkX <= maxX >> 4; chunkX++) {
                for (int chunkZ = minZ >> 4; chunkZ <= maxZ >> 4; chunkZ++) {
                    world.getChunkAt(chunkX, chunkZ).load();
                }
            }
        }

        public static int clamp0Max(int value, int max) {
            return Math.min(Math.max(value, 0), max);
        }
    }

    private static final Logger log = LoggerFactory.getLogger(GTARP.class);
    private WorkerPanelGenerator workerPanelGenerator;
    private OwnerMineGenerator ownerMineGenerator;

    private static final int WORK_MENU_SIZE = 27;
    private static final int JOBS_PER_PAGE = 21;
    private static final int PREV_BUTTON_SLOT = 21;
    private static final int PAGE_INFO_SLOT = 22;
    private static final int NEXT_BUTTON_SLOT = 23;
    private static final int WORK_LEVEL_BUTTON_SLOT = 24;
    private static final int WORK_SALARY_BUTTON_SLOT = 25;
    private static final int CLOSE_BUTTON_SLOT = 26;
    private static final String WORK_MENU_TITLE_TEXT = "Jobs";
    private static final String WORK_MENU_TITLE_TEMPLATE = WORK_MENU_TITLE_TEXT + " %s (%d)";
    private static final Pattern WORK_MENU_TITLE_PATTERN = Pattern.compile("^" + Pattern.quote(WORK_MENU_TITLE_TEXT) + " (?<worker>.+?) \\((?<page>\\d+)\\)$");
    private static final int ADMIN_MENU_SIZE = WORK_MENU_SIZE;
    private static final String ADMIN_MENU_TITLE_TEXT = "Mine Administration";
    private static final String ADMIN_MENU_TITLE_TEMPLATE = ADMIN_MENU_TITLE_TEXT + " (%d)";
    private static final Pattern ADMIN_MENU_TITLE_PATTERN = Pattern.compile("^" + Pattern.quote(ADMIN_MENU_TITLE_TEXT) + " \\((?<page>\\d+)\\)$");
    private static final PlainTextComponentSerializer PLAIN_SERIALIZER = PlainTextComponentSerializer.plainText();
    private static final int SALARY_XP_COST = 500;
    private static final int[] WORKER_LEVEL_COSTS = {0, 500, 1000, 1500, 2000, 2500};

    private NamespacedKey workJobKey;
    private Database database;

    private static final int OWNER_MENU_SIZE = 45;
    private static final int OWNER_SALARY_BLOCK_SLOT = 12;
    private static final int OWNER_XP_SLOT = 14;
    private static final int OWNER_LEVEL_START_SLOT = 28;
    private static final int OWNER_LEVEL_COUNT = 6;
    private static final int OWNER_CLOSE_SLOT = 44;
    private static final int BLOCK_SELECTION_MENU_SIZE = 27;
    private static final int BLOCK_SELECTION_BACK_SLOT = 26;
    private static final int OWNER_WORKERS_BUTTON_SLOT = 16;
    private static final int OWNER_WORKERS_MENU_SIZE = 45;
    private static final int OWNER_WORKERS_PER_PAGE = 36;
    private static final int OWNER_WORKERS_PREV_SLOT = 38;
    private static final int OWNER_WORKERS_PAGE_SLOT = 40;
    private static final int OWNER_WORKERS_NEXT_SLOT = 42;
    private static final int OWNER_WORKERS_BACK_SLOT = OWNER_CLOSE_SLOT;
    private static final int OWNER_WORKER_BONUS = 1000;
    private static final String OWNER_MENU_TITLE_TEXT = "Mine";
    private static final String OWNER_MENU_TITLE_TEMPLATE = OWNER_MENU_TITLE_TEXT + " %s";
    private static final Pattern OWNER_MENU_TITLE_PATTERN = Pattern.compile("^" + Pattern.quote(OWNER_MENU_TITLE_TEXT) + " (?<owner>.+?)(?<admin> \\*)?$");
    private static final String OWNER_SALARY_BLOCK_TITLE_TEXT = "Select Salary Block";
    private static final String OWNER_SALARY_BLOCK_TITLE_TEMPLATE = OWNER_SALARY_BLOCK_TITLE_TEXT + " %s";
    private static final Pattern OWNER_SALARY_TITLE_PATTERN = Pattern.compile("^" + Pattern.quote(OWNER_SALARY_BLOCK_TITLE_TEXT) + " (?<owner>.+?)(?<admin> \\*)?$");
    private static final String OWNER_WORKERS_TITLE_TEXT = "Workers";
    private static final String OWNER_WORKERS_TITLE_TEMPLATE = OWNER_WORKERS_TITLE_TEXT + " %s (%d)";
    private static final Pattern OWNER_WORKERS_TITLE_PATTERN = Pattern.compile("^" + Pattern.quote(OWNER_WORKERS_TITLE_TEXT) + " (?<owner>.+?) \\((?<page>\\d+)\\)(?<admin> \\*)?$");
    private static final String OWNER_TITLE_ADMIN_TAG = " *";

    private final List<Material> ownerSalaryBlockOptions = List.of(
            Material.COAL,
            Material.IRON_INGOT,
            Material.GOLD_INGOT,
            Material.EMERALD,
            Material.DIAMOND,
            Material.BREAD,
            Material.COBBLESTONE,
            Material.OAK_LOG,
            Material.COPPER_INGOT
    );

    private static class SpaceChunkGenerator extends ChunkGenerator {
        @Override
        public boolean canSpawn(@NotNull World world, int x, int z) {
            return true;
        }

        @Override
        public @NotNull Location getFixedSpawnLocation(@NotNull World world, @NotNull java.util.Random random) {
            return new Location(world, 0, 0, 0);
        }
    }

    public static class WorkerPanelGenerator {
        private static final int MODULE_SIZE = 10;
        private static final int MODULE_HEIGHT = 5;
        private static final int MAX_COLUMNS = 2;
        private static final int MAX_LEVELS = 10;
        private static final int FOUNDATION_Y = 64;
        private static final int BASE_OFFSET_X = 32;
        private static final int BASE_OFFSET_Z = 32;
        private static final int BUILDING_SPACING_X = MAX_COLUMNS * MODULE_SIZE + 6;
        private static final int BUILDING_SPACING_Z = MODULE_SIZE + 6;

        private int nextColumn;
        private int nextLevel;
        private int nextBuildingIndex;

        private final File configurationFile;

        public record ModulePlacement(BlockVector3 minCorner, BlockVector3 maxCorner, BlockVector3 interiorMin,
                                      BlockVector3 interiorMax, Location spawnLocation, String regionBaseId) {
        }

        public WorkerPanelGenerator(File path) throws IOException {
            configurationFile = path;
            FileConfiguration config = YamlConfiguration.loadConfiguration(configurationFile);
            nextColumn = GeneratorUtils.clamp0Max(config.getInt("nextColumn", 0), MAX_COLUMNS - 1);
            nextLevel = GeneratorUtils.clamp0Max(config.getInt("nextLevel", 0), MAX_LEVELS - 1);
            nextBuildingIndex = Math.max(0, config.getInt("nextBuildingIndex", 0));
            if (!configurationFile.exists()) {
                saveState();
            }
        }

        public synchronized ModulePlacement generateNextRoom(World world) throws IOException {
            GeneratorUtils.GridCoordinates gridPos = GeneratorUtils.calculateDiagonalGridPosition(nextBuildingIndex);

            BlockVector2 gridBase = BlockVector2.at(
                    BASE_OFFSET_X + gridPos.x * BUILDING_SPACING_X,
                    BASE_OFFSET_Z + gridPos.z * BUILDING_SPACING_Z
            );
            int originX = gridBase.x();
            int originZ = gridBase.z();

            if (nextColumn == 0 && nextLevel == 0) {
                int maxX = originX + MAX_COLUMNS * MODULE_SIZE - 1;
                int maxZ = originZ + MODULE_SIZE - 1;
                GeneratorUtils.ensureChunksLoaded(world, originX, maxX, originZ, maxZ);

                for (int x = originX; x <= maxX; x++) {
                    for (int z = originZ; z <= maxZ; z++) {
                        world.getBlockAt(x, FOUNDATION_Y - 1, z).setType(Material.STONE_BRICKS, false);
                    }
                }
            }

            int minX = originX + nextColumn * MODULE_SIZE;
            int minY = FOUNDATION_Y + nextLevel * MODULE_HEIGHT;
            int maxX = minX + MODULE_SIZE - 1;
            int maxY = minY + MODULE_HEIGHT - 1;
            int maxZ = originZ + MODULE_SIZE - 1;

            GeneratorUtils.ensureChunksLoaded(world, minX, maxX, originZ, maxZ);

            for (int x = minX; x <= maxX; x++) {
                for (int y = minY; y <= maxY; y++) {
                    for (int z = originZ; z <= maxZ; z++) {
                        boolean isWestWall = x == minX;
                        boolean isEastWall = x == maxX;
                        boolean isNorthWall = z == originZ;
                        boolean isSouthWall = z == maxZ;

                        if (y == minY) {
                            world.getBlockAt(x, y, z).setType(Material.SMOOTH_STONE, false);
                            continue;
                        }

                        if (y == maxY) {
                            world.getBlockAt(x, y, z).setType(Material.BRICKS, false);
                            continue;
                        }

                        if (!(isWestWall || isEastWall || isNorthWall || isSouthWall)) {
                            world.getBlockAt(x, y, z).setType(Material.AIR, false);
                            continue;
                        }

                        boolean hasWestNeighbor = nextColumn > 0;
                        boolean hasEastNeighbor = nextColumn < MAX_COLUMNS - 1;
                        boolean inWindowHeight = y >= minY + 2 && y <= minY + 3;

                        if (inWindowHeight &&
                                (!isWestWall || !hasWestNeighbor) &&
                                (!isEastWall || !hasEastNeighbor) &&
                                ((isWestWall || isEastWall) ?
                                        (z >= originZ + 1 && z <= maxZ - 1) :
                                        (x >= minX + 1 && x <= maxX - 1))) {
                            world.getBlockAt(x, y, z).setType(Material.GLASS, false);
                        } else {
                            world.getBlockAt(x, y, z).setType(Material.BRICKS, false);
                        }
                    }
                }
            }

            String regionBaseId = String.format(
                    Locale.ROOT,
                    "wp_%d_%d_%d_%d",
                    gridPos.x,
                    gridPos.z,
                    nextColumn,
                    nextLevel
            );

            nextColumn += 1;
            if (nextColumn >= MAX_COLUMNS) {
                nextColumn = 0;
                nextLevel += 1;
                if (nextLevel >= MAX_LEVELS) {
                    nextLevel = 0;
                    nextBuildingIndex += 1;
                }
            }
            saveState();
            return new ModulePlacement(
                    BlockVector3.at(minX, minY, originZ),
                    BlockVector3.at(maxX, maxY, maxZ),
                    BlockVector3.at(minX + 1, minY + 1, originZ + 1),
                    BlockVector3.at(maxX - 1, maxY - 1, maxZ - 1),
                    new Location(
                            world,
                            minX + ((MODULE_SIZE - 1) / 2.0) + 0.5,
                            minY + 1,
                            originZ + ((MODULE_SIZE - 1) / 2.0) + 0.5,
                            180f,
                            0f
                    ),
                    regionBaseId
            );
        }

        private void saveState() throws IOException {
            java.util.Map<String, Integer> states = new java.util.HashMap<>();
            states.put("nextColumn", nextColumn);
            states.put("nextLevel", nextLevel);
            states.put("nextBuildingIndex", nextBuildingIndex);
            FileConfiguration config = new YamlConfiguration();
            for (Map.Entry<String, Integer> entry : states.entrySet()) {
                config.set(entry.getKey(), entry.getValue());
            }
            File parent = configurationFile.getParentFile();
            if (parent != null && !parent.exists()) {
                parent.mkdirs();
            }
            config.save(configurationFile);
        }

    }

    public static class OwnerMineGenerator {
        private static final int SURFACE_Y = 64;
        private static final int MINE_DEPTH = 32;
        private static final int MINE_SIZE = 32;
        private static final int BASE_OFFSET_X = 32;
        private static final int BASE_OFFSET_Z = 32;
        private static final int SPACING_X = MINE_SIZE + 8;
        private static final int SPACING_Z = MINE_SIZE + 8;

        private int nextBuildingIndex;
        private final File configurationFile;

        public record ModulePlacement(
                BlockVector3 shellMin,
                BlockVector3 shellMax,
                BlockVector3 interiorMin,
                BlockVector3 interiorMax,
                BlockVector3 spiralStairsMin,
                BlockVector3 spiralStairsMax,
                Location spawnLocation,
                String regionBaseId
        ) {
        }

        public OwnerMineGenerator(File path) throws IOException {
            configurationFile = path;
            FileConfiguration config = YamlConfiguration.loadConfiguration(configurationFile);
            nextBuildingIndex = Math.max(0, config.getInt("nextBuildingIndex", 0));
            if (!configurationFile.exists()) {
                saveState();
            }
        }

        public synchronized ModulePlacement generateNextMine(World world) throws IOException {
            // Diagonal enumeration to fill negative quadrant x<0, z<0
            GeneratorUtils.GridCoordinates gridPos = GeneratorUtils.calculateDiagonalGridPosition(nextBuildingIndex);

            int minX = -(BASE_OFFSET_X + gridPos.x * SPACING_X);
            int minZ = -(BASE_OFFSET_Z + gridPos.z * SPACING_Z);
            int maxX = minX + MINE_SIZE - 1;
            int maxZ = minZ + MINE_SIZE - 1;

            int surfaceY = SURFACE_Y;
            int bottomY = SURFACE_Y - MINE_DEPTH;

            GeneratorUtils.ensureChunksLoaded(world, minX, maxX, minZ, maxZ);

            // Prepare ground and perimeter walls
            for (int x = minX; x <= maxX; x++) {
                for (int z = minZ; z <= maxZ; z++) {
                    // base floor under grass
                    world.getBlockAt(x, surfaceY - 1, z).setType(Material.DIRT, false);
                    // top grass layer
                    world.getBlockAt(x, surfaceY, z).setType(Material.GRASS_BLOCK, false);
                    // clear air above
                    for (int y = surfaceY + 1; y <= surfaceY + 3; y++) {
                        world.getBlockAt(x, y, z).setType(Material.AIR, false);
                    }
                    // perimeter walls (3 blocks high)
                    if (x == minX || x == maxX || z == minZ || z == maxZ) {
                        for (int y = surfaceY + 1; y <= surfaceY + 3; y++) {
                            world.getBlockAt(x, y, z).setType(Material.STONE_BRICKS, false);
                        }
                    }
                }
            }

            // Underground mine volume: fill with stone first
            for (int x = minX + 1; x <= maxX - 1; x++) {
                for (int z = minZ + 1; z <= maxZ - 1; z++) {
                    for (int y = bottomY; y <= surfaceY - 1; y++) {
                        world.getBlockAt(x, y, z).setType(Material.GOLD_ORE, false);
                    }
                }
            }

            // Bottom unbreakable floor layer
            for (int x = minX + 1; x <= maxX - 1; x++) {
                for (int z = minZ + 1; z <= maxZ - 1; z++) {
                    world.getBlockAt(x, bottomY, z).setType(Material.BEDROCK, false);
                }
            }

            // Underground perimeter walls (from bottom to surface)
            // These are protected by the WorldGuard shell region; players can only break inside the interior region.
            for (int x = minX; x <= maxX; x++) {
                for (int y = bottomY + 1; y <= surfaceY - 1; y++) {
                    world.getBlockAt(x, y, minZ).setType(Material.STONE_BRICKS, false);
                    world.getBlockAt(x, y, maxZ).setType(Material.STONE_BRICKS, false);
                }
            }
            for (int z = minZ; z <= maxZ; z++) {
                for (int y = bottomY + 1; y <= surfaceY - 1; y++) {
                    world.getBlockAt(minX, y, z).setType(Material.STONE_BRICKS, false);
                    world.getBlockAt(maxX, y, z).setType(Material.STONE_BRICKS, false);
                }
            }

            // Larger hut for spiral staircase access
            int houseMinX = minX + 2;
            int houseMinZ = minZ + 2;
            int houseMaxX = houseMinX + 6;
            int houseMaxZ = houseMinZ + 6;
            for (int x = houseMinX; x <= houseMaxX; x++) {
                for (int z = houseMinZ; z <= houseMaxZ; z++) {
                    // floor stays grass
                    for (int y = surfaceY + 1; y <= surfaceY + 3; y++) {
                        boolean isWall = (x == houseMinX || x == houseMaxX || z == houseMinZ || z == houseMaxZ);
                        if (isWall) {
                            boolean doorway = (z == houseMaxZ && (x >= houseMinX + 2 && x <= houseMaxX - 2) && (y == surfaceY + 1 || y == surfaceY + 2));
                            if (!doorway) {
                                world.getBlockAt(x, y, z).setType(Material.OAK_PLANKS, false);
                            } else {
                                world.getBlockAt(x, y, z).setType(Material.AIR, false);
                            }
                        } else {
                            world.getBlockAt(x, y, z).setType(Material.AIR, false);
                        }
                    }
                }
            }
            // Roof
            for (int x = houseMinX; x <= houseMaxX; x++) {
                for (int z = houseMinZ; z <= houseMaxZ; z++) {
                    world.getBlockAt(x, surfaceY + 4, z).setType(Material.OAK_PLANKS, false);
                }
            }

            int centerX = (houseMinX + houseMaxX) / 2;
            int centerZ = (houseMinZ + houseMaxZ) / 2;
            int spiralMinX = centerX - 1;
            int spiralMaxX = centerX + 1;
            int spiralMinZ = centerZ - 1;
            int spiralMaxZ = centerZ + 1;

            for (int x = spiralMinX; x <= spiralMaxX; x++) {
                for (int z = spiralMinZ; z <= spiralMaxZ; z++) {
                    for (int y = bottomY + 1; y <= surfaceY; y++) {
                        world.getBlockAt(x, y, z).setType(Material.AIR, false);
                    }
                }
            }

            int[] spiralX = {0, 1, 0, -1};
            int[] spiralZ = {-1, 0, 1, 0};
            int[] cornerX = {1, 1, -1, -1};
            int[] cornerZ = {-1, 1, 1, -1};

            org.bukkit.block.BlockFace[] facings = {
                    org.bukkit.block.BlockFace.WEST,
                    org.bukkit.block.BlockFace.NORTH,
                    org.bukkit.block.BlockFace.EAST,
                    org.bukkit.block.BlockFace.SOUTH
            };

            int currentY = surfaceY - 1;
            int patternIndex = 0;

            while (bottomY < currentY) {
                int stepX = centerX + spiralX[patternIndex];
                int stepZ = centerZ + spiralZ[patternIndex];

                org.bukkit.block.Block stairBlock = world.getBlockAt(stepX, currentY + 1, stepZ);
                stairBlock.setType(Material.STONE_BRICK_STAIRS, false);

                if (stairBlock.getBlockData() instanceof org.bukkit.block.data.type.Stairs stairs) {
                    stairs.setFacing(facings[patternIndex]);
                    stairBlock.setBlockData(stairs, false);
                }

                int cornerStepX = centerX + cornerX[patternIndex];
                int cornerStepZ = centerZ + cornerZ[patternIndex];
                world.getBlockAt(cornerStepX, currentY, cornerStepZ).setType(Material.STONE_BRICKS, false);

                world.getBlockAt(centerX, currentY, centerZ).setType(Material.GLOWSTONE, false);

                patternIndex = (patternIndex + 1) % 4;
                currentY--;
            }

            world.getBlockAt(centerX + cornerX[3], surfaceY, centerZ + cornerZ[3]).setType(Material.STONE_BRICKS, false);

            BlockVector3 interiorMin = BlockVector3.at(minX + 1, bottomY + 1, minZ + 1);
            BlockVector3 interiorMax = BlockVector3.at(maxX - 1, surfaceY - 1, maxZ - 1);
            BlockVector3 spiralStairsMin = BlockVector3.at(spiralMinX, bottomY + 1, spiralMinZ);
            BlockVector3 spiralStairsMax = BlockVector3.at(spiralMaxX, surfaceY - 1, spiralMaxZ);

            nextBuildingIndex += 1;
            saveState();

            return new ModulePlacement(
                    BlockVector3.at(minX, bottomY, minZ),
                    BlockVector3.at(maxX, surfaceY + 4, maxZ),
                    interiorMin,
                    interiorMax,
                    spiralStairsMin,
                    spiralStairsMax,
                    new Location(world, (houseMinX + houseMaxX) / 2.0 + 0.5, surfaceY + 1, houseMinZ + 1.5, 180f, 0f),
                    String.format(Locale.ROOT,
                            "owner_mine_%d_%d",
                            gridPos.x,
                            gridPos.z)
            );
        }

        private void saveState() throws IOException {
            FileConfiguration config = new YamlConfiguration();
            config.set("nextBuildingIndex", nextBuildingIndex);
            File parent = configurationFile.getParentFile();
            if (parent != null && !parent.exists()) {
                parent.mkdirs();
            }
            config.save(configurationFile);
        }
    }

    private static final class OwnerState {
        private final String ownerName;
        private final Material salaryBlock;
        private final int xpPerBlock;
        private final List<String> levelTexts;
        private final double spawnX;
        private final double spawnY;
        private final double spawnZ;

        private OwnerState(String ownerName,
                           Material salaryBlock,
                           int xpPerBlock,
                           List<String> levelTexts,
                           double spawnX,
                           double spawnY,
                           double spawnZ) {
            this.ownerName = ownerName;
            this.salaryBlock = salaryBlock;
            this.xpPerBlock = xpPerBlock;
            this.levelTexts = List.copyOf(levelTexts);
            this.spawnX = spawnX;
            this.spawnY = spawnY;
            this.spawnZ = spawnZ;
        }

        private String ownerName() {
            return ownerName;
        }

        private Material salaryBlock() {
            return salaryBlock;
        }

        private int xpPerBlock() {
            return xpPerBlock;
        }

        private String getLevelText(int level) {
            return levelTexts.get(level);
        }

        private String oreRegionId() {
            return regionId("ore");
        }

        private String shellRegionId() {
            return regionId("shell");
        }

        private String stairsRegionId() {
            return regionId("stairs");
        }

        private Location spawnLocation() {
            return new Location(Bukkit.getWorld("world"), spawnX, spawnY, spawnZ);
        }

        private String regionId(String suffix) {
            return ownerName + "_" + suffix;
        }
    }

    private record OwnerWorkerEntry(String workerName, int experience, int level) {
    }

    private record WorkerBonusResult(int newExperience, boolean active) {
    }

    private static final class WorkerState {
        private final String workerName;
        private final String employerName;
        private final Map<String, EmployerStats> statsByEmployer;
        private final int currentExperience;
        private final int currentLevel;

        private WorkerState(String workerName,
                            String employerName,
                            Map<String, EmployerStats> statsByEmployer,
                            int currentExperience,
                            int currentLevel) {
            this.workerName = workerName;
            this.employerName = employerName;
            this.statsByEmployer = Map.copyOf(statsByEmployer);
            this.currentExperience = currentExperience;
            this.currentLevel = currentLevel;
        }

        private String workerName() {
            return workerName;
        }

        private String employerName() {
            return employerName;
        }

        private int currentExperience() {
            return currentExperience;
        }

        private int currentLevel() {
            return currentLevel;
        }

        private Map<String, EmployerStats> statsByEmployer() {
            return statsByEmployer;
        }

        private int experience(String employerName) {
            return statsFor(employerName).experience();
        }

        private int level(String employerName) {
            return statsFor(employerName).level();
        }

        private EmployerStats statsFor(String employerName) {
            return statsByEmployer.getOrDefault(employerName, EmployerStats.EMPTY);
        }

        private static final class EmployerStats {
            private static final EmployerStats EMPTY = new EmployerStats(0, 0);
            private final int experience;
            private final int level;

            private EmployerStats(int experience, int level) {
                this.experience = experience;
                this.level = level;
            }

            private int experience() {
                return experience;
            }

            private int level() {
                return level;
            }

            private EmployerStats withExperience(int value) {
                if (value == experience) {
                    return this;
                }
                return new EmployerStats(value, level);
            }

            private EmployerStats withLevel(int value) {
                if (value == level) {
                    return this;
                }
                return new EmployerStats(experience, value);
            }
        }
    }

    private static final class Database {
        private final ConnectionPool connectionPool;
        private final ConnectionFactory connectionFactory;
        private static final String[] LEVEL_COLUMNS = {
                "level0_message",
                "level1_message",
                "level2_message",
                "level3_message",
                "level4_message",
                "level5_message"
        };

        private Database() {
            String password = envOrDefault("DB_PASSWORD", "");

            PostgresqlConnectionConfiguration.Builder builder = PostgresqlConnectionConfiguration.builder()
                    .host(envOrDefault("DB_HOST", "localhost"))
                    .port(envIntOrDefault("DB_PORT", 5432))
                    .username(envOrDefault("DB_USER", "postgres"))
                    .database(envOrDefault("DB_NAME", "postgres"))
                    .applicationName("GTARP");
            if (!password.isEmpty()) {
                builder.password(password);
            }

            this.connectionPool = new ConnectionPool(ConnectionPoolConfiguration.builder(new PostgresqlConnectionFactory(builder.build()))
                    .initialSize(3)
                    .maxSize(3)
                    .maxIdleTime(Duration.ofSeconds(300))
                    .validationQuery("SELECT 1 FROM mine_owners")
                    .name("gtarp-postgres")
                    .build());
            this.connectionFactory = connectionPool;
            Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    connection -> Mono.from(connection.validate(ValidationDepth.REMOTE))
                            .flatMap(valid -> valid
                                    ? Mono.<Void>empty()
                                    : Mono.error(new IllegalStateException("PostgreSQL connection validation failed"))),
                    connection -> Mono.from(connection.close())
            ).block();
        }

        private static String envOrDefault(String key, String fallback) {
            String value = System.getenv(key);
            return value == null || value.isBlank() ? fallback : value.trim();
        }

        private static int envIntOrDefault(String key, int fallback) {
            String value = System.getenv(key);
            if (value == null || value.isBlank()) {
                return fallback;
            }
            try {
                return Integer.parseInt(value.trim());
            } catch (NumberFormatException exception) {
                log.warn("Invalid integer value for {} ({}). Falling back to {}.", key, value, fallback);
                return fallback;
            }
        }

        List<OwnerState> loadOwners() {
            return Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    (Function<Connection, Mono<List<OwnerState>>>) connection -> Flux.from(connection.createStatement("""
                                            SELECT mo.owner_name,
                                                   mo.salary_block,
                                                   mo.xp_per_block,
                                                   mo.spawn_x,
                                                   mo.spawn_y,
                                                   mo.spawn_z,
                                                   mo.level0_message,
                                                   mo.level1_message,
                                                   mo.level2_message,
                                                   mo.level3_message,
                                                   mo.level4_message,
                                                   mo.level5_message
                                            FROM mine_owners mo
                                            JOIN users u ON u.username = mo.owner_name
                                            WHERE NOT (
                                                COALESCE(mo.spawn_x, 0) = 0
                                                AND COALESCE(mo.spawn_y, 0) = 0
                                                AND COALESCE(mo.spawn_z, 0) = 0
                                            )
                                            ORDER BY u.registered_at DESC
                                    """).execute())
                            .flatMap(result -> result.map((row, metadata) -> toOwnerState(row)))
                            .collectList(),
                    connection1 -> Mono.from(connection1.close())
            ).block();
        }

        List<OwnerWorkerEntry> loadOwnerWorkers(String ownerName) {
            return Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    (Function<Connection, Mono<List<OwnerWorkerEntry>>>) connection -> Flux.from(connection.createStatement("""
                                            SELECT worker_name, experience, level
                                            FROM worker_employments
                                            WHERE employer_name = $1 AND is_active = TRUE
                                            ORDER BY experience DESC, worker_name
                                    """)
                                    .bind(0, ownerName)
                                    .execute())
                            .flatMap(result -> result.map((row, metadata) -> new OwnerWorkerEntry(
                                    row.get("worker_name", String.class),
                                    Optional.ofNullable(row.get("experience", Integer.class)).orElse(0),
                                    Optional.ofNullable(row.get("level", Integer.class)).orElse(0)
                            )))
                            .collectList(),
                    connection1 -> Mono.from(connection1.close())
            ).block();
        }

        OwnerState findOwner(String ownerName) {
            return Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    (Function<Connection, Mono<OwnerState>>) connection -> Flux.from(connection.createStatement("""
                                                    SELECT mo.owner_name,
                                                           mo.salary_block,
                                                           mo.xp_per_block,
                                                           mo.spawn_x,
                                                           mo.spawn_y,
                                                           mo.spawn_z,
                                                           mo.level0_message,
                                                           mo.level1_message,
                                                           mo.level2_message,
                                                           mo.level3_message,
                                                           mo.level4_message,
                                                           mo.level5_message
                                                    FROM mine_owners mo
                                                    WHERE mo.owner_name = $1
                                            """)
                                    .bind(0, ownerName)
                                    .execute())
                            .flatMap(result -> result.map((row, metadata) -> toOwnerState(row)))
                            .single(),
                    connection1 -> Mono.from(connection1.close())
            ).block();
        }

        int ownerCount() {
            Number count = Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    (Function<Connection, Mono<Number>>) connection -> Flux.from(connection.createStatement("SELECT COUNT(*) FROM mine_owners").execute())
                            .flatMap(result -> result.map((row, metadata) -> row.get(0, Number.class)))
                            .single(),
                    connection1 -> Mono.from(connection1.close())
            ).block();
            return count == null ? 0 : count.intValue();
        }

        boolean ownerExists(String ownerName) {
            return Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    (Function<Connection, Mono<Boolean>>) connection -> Flux.from(connection.createStatement("""
                                                    SELECT 1
                                                    FROM mine_owners
                                                    WHERE owner_name = $1
                                            """)
                                    .bind(0, ownerName)
                                    .execute())
                            .flatMap(result -> result.map((row, metadata) -> Boolean.TRUE))
                            .next()
                            .defaultIfEmpty(false),
                    connection1 -> Mono.from(connection1.close())
            ).block();
        }

        WorkerState findWorker(String workerName) {
            return Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    (Function<Connection, Mono<WorkerState>>) connection -> Flux.from(connection.createStatement("""
                                                    SELECT employer_name, experience, level, is_active
                                                    FROM worker_employments
                                                    WHERE worker_name = $1
                                            """)
                                    .bind(0, workerName)
                                    .execute())
                            .flatMap(result -> result.map((row, metadata) -> new WorkerRow(
                                    row.get("employer_name", String.class),
                                    Optional.ofNullable(row.get("experience", Integer.class)).orElse(0),
                                    Optional.ofNullable(row.get("level", Integer.class)).orElse(0),
                                    Boolean.TRUE.equals(row.get("is_active", Boolean.class))
                            )))
                            .collectList()
                            .map(rows -> {
                                Map<String, WorkerState.EmployerStats> stats = new HashMap<>();
                                String activeEmployer = null;
                                int activeExperience = 0;
                                int activeLevel = 0;
                                for (WorkerRow row : rows) {
                                    stats.put(row.employerName(), new WorkerState.EmployerStats(row.experience(), row.level()));
                                    if (row.active()) {
                                        activeEmployer = row.employerName();
                                        activeExperience = row.experience();
                                        activeLevel = row.level();
                                    }
                                }
                                return new WorkerState(workerName, activeEmployer, stats, activeExperience, activeLevel);
                            }),
                    connection1 -> Mono.from(connection1.close())
            ).block();
        }

        void updateWorkerExperience(String workerName, int experience) {
            Mono.usingWhen(
                    Mono.from(connectionFactory.create()),
                    (Function<Connection, Mono<Void>>) connection -> Flux.from(connection.createStatement("""
                                                    UPDATE worker_employments
                                                    SET experience = $1
                                                    WHERE worker_name = $2 AND is_active = TRUE
                                            """)
                                    .bind(0, experience)
                                    .bind(1, workerName)
                                    .execute())
                            .flatMap(Result::getRowsUpdated)
                            .then(),
                    connection1 -> Mono.from(connection1.close())
            ).block();
        }

        private OwnerState toOwnerState(Row row) {
            OwnerRow ownerRow = new OwnerRow(
                    Objects.requireNonNull(row.get("owner_name", String.class)),
                    parseMaterial(row.get("salary_block", String.class)),
                    Optional.ofNullable(row.get("xp_per_block", Integer.class)).orElse(0),
                    Optional.ofNullable(row.get("spawn_x", Double.class)).orElse(0.0),
                    Optional.ofNullable(row.get("spawn_y", Double.class)).orElse(0.0),
                    Optional.ofNullable(row.get("spawn_z", Double.class)).orElse(0.0)
            );
            for (int index = 0; index < LEVEL_COLUMNS.length; index++) {
                ownerRow.setLevel(index, row.get(LEVEL_COLUMNS[index], String.class));
            }
            return ownerRow.toOwnerState();
        }

        private static Material parseMaterial(String value) {
            if (value == null || value.isBlank()) {
                return Material.GOLD_INGOT;
            }
            Material material = Material.matchMaterial(value.trim());
            return material != null ? material : Material.GOLD_INGOT;
        }

        private record WorkerRow(String employerName, int experience, int level, boolean active) {
        }

        private static final class OwnerRow {
            private final String ownerName;
            private final Material salaryBlock;
            private final int xpPerBlock;
            private final double spawnX;
            private final double spawnY;
            private final double spawnZ;
            private final List<String> levelTexts = new ArrayList<>();

            private OwnerRow(String ownerName,
                             Material salaryBlock,
                             int xpPerBlock,
                             double spawnX,
                             double spawnY,
                             double spawnZ) {
                this.ownerName = ownerName;
                this.salaryBlock = salaryBlock;
                this.xpPerBlock = xpPerBlock;
                this.spawnX = spawnX;
                this.spawnY = spawnY;
                this.spawnZ = spawnZ;
                for (int i = 0; i < OWNER_LEVEL_COUNT; i++) {
                    levelTexts.add("");
                }
            }

            private void setLevel(int index, String message) {
                if (0 <= index && index < levelTexts.size()) {
                    levelTexts.set(index, message == null ? "" : message);
                }
            }

            private OwnerState toOwnerState() {
                return new OwnerState(
                        ownerName,
                        salaryBlock,
                        xpPerBlock,
                        levelTexts,
                        spawnX,
                        spawnY,
                        spawnZ
                );
            }
        }
    }
    private void openOwnerMenu(Player viewer, String ownerName, boolean fromAdmin) {
        runAsync(() -> database.findOwner(ownerName), state -> renderOwnerMenu(viewer, state, fromAdmin), error -> {
            log.error("Failed to open owner menu for {} (owner {})", viewer.getName(), ownerName, error);
        });
    }

    private void renderOwnerMenu(Player viewer, OwnerState state, boolean fromAdmin) {
        Inventory inventory = Bukkit.createInventory(null, OWNER_MENU_SIZE, Component.text(formatOwnerContextTitle(OWNER_MENU_TITLE_TEMPLATE, state.ownerName(), fromAdmin)));

        inventory.setItem(OWNER_SALARY_BLOCK_SLOT, createSimpleItem(
                state.salaryBlock(),
                "Payout item: " + state.salaryBlock().name(),
                List.of("Click to choose the reward item")
        ));

        inventory.setItem(OWNER_XP_SLOT, createSimpleItem(
                Material.EXPERIENCE_BOTTLE,
                "XP per block",
                List.of(
                        "Current reward: " + state.xpPerBlock(),
                        "Left click: +1, Shift+Left: +10",
                        "Right click: -1, Shift+Right: -10"
                )
        ));

        inventory.setItem(OWNER_WORKERS_BUTTON_SLOT, createSimpleItem(
                Material.PLAYER_HEAD,
                "Manage workers",
                List.of("Click to view employees", "Grant bonuses directly")
        ));

        inventory.setItem(10, createSimpleItem(
                Material.NAME_TAG,
                "Managing: " + state.ownerName(),
                List.of("Mine owner:", state.ownerName())
        ));

        for (int level = 0; level < OWNER_LEVEL_COUNT; level++) {
            List<String> lore = new ArrayList<>();
            lore.add("Current text:");
            lore.addAll(wrapText(state.getLevelText(level), 32));
            lore.add(" ");
            lore.add("Click to edit level " + level + " message");
            inventory.setItem(OWNER_LEVEL_START_SLOT + level, createSimpleItem(
                    Material.BOOK,
                    "Level " + level + " message",
                    lore
            ));
        }

        inventory.setItem(OWNER_CLOSE_SLOT, createSimpleItem(
                fromAdmin ? Material.ARROW : Material.BARRIER,
                fromAdmin ? "Back to admin menu" : "Close menu",
                fromAdmin
                        ? List.of("Return to mine administration")
                        : List.of("Return to gameplay")
        ));
        viewer.openInventory(inventory);
    }

    private void openOwnerWorkersMenu(Player viewer, String ownerName, boolean adminView, int requestedPage) {
        runAsync(() -> database.loadOwnerWorkers(ownerName), workers -> renderOwnerWorkersMenu(viewer, ownerName, adminView, workers, requestedPage), error -> {
            log.error("Failed to open worker list for {} (owner {})", viewer.getName(), ownerName, error);
        });
    }

    private void renderOwnerWorkersMenu(Player viewer, String ownerName, boolean adminView, List<OwnerWorkerEntry> workers, int requestedPage) {
        int maxPage = workers.isEmpty() ? 0 : (workers.size() - 1) / OWNER_WORKERS_PER_PAGE;
        int page = Math.max(0, Math.min(requestedPage, maxPage));
        int startIndex = page * OWNER_WORKERS_PER_PAGE;
        int endIndex = Math.min(startIndex + OWNER_WORKERS_PER_PAGE, workers.size());

        String title = String.format(Locale.ROOT, OWNER_WORKERS_TITLE_TEMPLATE, ownerName, page + 1);
        if (adminView) {
            title += OWNER_TITLE_ADMIN_TAG;
        }

        Inventory inventory = Bukkit.createInventory(null, OWNER_WORKERS_MENU_SIZE, Component.text(title));

        int slot = 0;
        for (int index = startIndex; index < endIndex; index++, slot++) {
            OwnerWorkerEntry worker = workers.get(index);
            ItemStack item = new ItemStack(Material.PLAYER_HEAD);
            ItemMeta meta = item.getItemMeta();
            if (meta != null) {
                meta.displayName(Component.text("Worker: " + worker.workerName()));
                List<Component> lore = new ArrayList<>();
                lore.add(Component.text("XP: " + worker.experience()));
                lore.add(Component.text("Level: " + worker.level()));
                lore.add(Component.text("Click to grant " + OWNER_WORKER_BONUS + " XP bonus"));
                meta.lore(lore);
                meta.getPersistentDataContainer().set(workJobKey, PersistentDataType.STRING, worker.workerName());
                if (meta instanceof SkullMeta skullMeta) {
                    skullMeta.setOwningPlayer(Bukkit.getOfflinePlayer(worker.workerName()));
                }
                item.setItemMeta(meta);
            }
            inventory.setItem(slot, item);
        }

        if (workers.isEmpty()) {
            inventory.setItem(13, createSimpleItem(
                    Material.BOOK,
                    "No active workers",
                    List.of("Hire employees to see them here")
            ));
        }

        inventory.setItem(OWNER_WORKERS_PREV_SLOT, page > 0
                ? createSimpleItem(Material.ARROW, "Previous Page", List.of("View earlier workers"))
                : createSimpleItem(Material.GRAY_STAINED_GLASS_PANE, "No previous page", List.of("Already on the first page"))
        );

        inventory.setItem(OWNER_WORKERS_NEXT_SLOT, page < maxPage && !workers.isEmpty()
                ? createSimpleItem(Material.ARROW, "Next Page", List.of("View more workers"))
                : createSimpleItem(Material.GRAY_STAINED_GLASS_PANE, "No next page", List.of("Already on the last page"))
        );

        inventory.setItem(OWNER_WORKERS_PAGE_SLOT, createSimpleItem(
                Material.MAP,
                "Page " + (page + 1) + " of " + (maxPage + 1),
                List.of(workers.isEmpty()
                        ? "No workers to display"
                        : "Showing " + (startIndex + 1) + "-" + endIndex + " of " + workers.size())
        ));

        inventory.setItem(OWNER_WORKERS_BACK_SLOT, createSimpleItem(
                Material.ARROW,
                "Back to mine menu",
                List.of("Return to mine management")
        ));

        viewer.openInventory(inventory);
    }

    private String formatOwnerContextTitle(String template, String ownerName, boolean adminView) {
        String formatted = String.format(Locale.ROOT, template, ownerName);
        return adminView ? formatted + OWNER_TITLE_ADMIN_TAG : formatted;
    }

    private record OwnerMenuContext(String ownerName, boolean adminView) {
    }

    private List<String> wrapText(String text, int lineLength) {
        List<String> lines = new ArrayList<>();
        if (text == null || text.isBlank()) {
            lines.add("(not set)");
            return lines;
        }
        String[] words = text.split("\\s+");
        StringBuilder current = new StringBuilder();
        for (String word : words) {
            if (current.length() + word.length() + 1 > lineLength) {
                if (!current.isEmpty()) {
                    lines.add(current.toString());
                    current.setLength(0);
                }
            }
            if (!current.isEmpty()) {
                current.append(' ');
            }
            current.append(word);
        }
        if (!current.isEmpty()) {
            lines.add(current.toString());
        }
        return lines;
    }

    private void openWorkMenu(Player viewer, String workerName, int requestedPage) {
        runAsync(() -> {
            WorkerState workerState = database.findWorker(workerName);
            List<OwnerState> owners = database.loadOwners();
            String activeEmployerName = workerState.employerName();
            OwnerState currentEmployer = null;
            if (activeEmployerName != null) {
                currentEmployer = database.findOwner(activeEmployerName);
            }
            return new WorkMenuData(workerState, owners, currentEmployer);
        }, data -> renderWorkMenu(viewer, data, requestedPage), error -> {
            log.error("Failed to open work menu for {}", workerName, error);
        });
    }

    private void renderWorkMenu(Player viewer, WorkMenuData data, int requestedPage) {
        WorkerState workerState = data.workerState();
        List<OwnerState> owners = data.owners();
        OwnerState currentEmployer = data.currentEmployer();
        int maxPage = owners.isEmpty() ? 0 : (owners.size() - 1) / JOBS_PER_PAGE;
        int page = Math.max(0, Math.min(requestedPage, maxPage));
        int startIndex = page * JOBS_PER_PAGE;
        int endIndex = Math.min(startIndex + JOBS_PER_PAGE, owners.size());

        Inventory inventory = Bukkit.createInventory(
                null,
                WORK_MENU_SIZE,
                Component.text(String.format(Locale.ROOT, WORK_MENU_TITLE_TEMPLATE, workerState.workerName(), page + 1))
        );

        int currentExperience = viewer.getTotalExperience();
        IntFunction<List<String>> xpStatusLines = requiredExperience -> {
            List<String> lines = new ArrayList<>(2);
            lines.add("Your XP: " + currentExperience);
            if (requiredExperience <= currentExperience) {
                lines.add("You have enough XP.");
            } else {
                lines.add("Need " + (requiredExperience - currentExperience) + " more XP.");
            }
            return lines;
        };
        String activeEmployerName = workerState.employerName();

        int slot = 0;
        for (int jobIndex = startIndex; jobIndex < endIndex; jobIndex++, slot++) {
            OwnerState owner = owners.get(jobIndex);
            ItemStack item = new ItemStack(Material.PAPER);
            ItemMeta meta = item.getItemMeta();
            if (meta != null) {
                meta.displayName(Component.text("Owner: " + owner.ownerName()));
                List<Component> lore = new ArrayList<>();
                lore.add(Component.text("Payout: " + owner.salaryBlock().name()));
                lore.add(Component.text("XP per block: " + owner.xpPerBlock()));
                if (owner.ownerName().equals(activeEmployerName)) {
                    lore.add(Component.text("Click to teleport"));
                } else {
                    lore.add(Component.text("Click to start working"));
                }
                meta.lore(lore);
                meta.getPersistentDataContainer().set(workJobKey, PersistentDataType.STRING, owner.ownerName());
                item.setItemMeta(meta);
            }
            inventory.setItem(slot, item);
        }

        if (owners.isEmpty()) {
            inventory.setItem(WORK_MENU_SIZE / 2, createSimpleItem(
                    Material.BOOK,
                    "No employers available",
                    List.of("Wait for owners to join the game.")
            ));
        }

        if (currentEmployer != null) {
            int currentLevel = workerState.level(currentEmployer.ownerName());
            int clampedLevelIndex = Math.max(0, Math.min(currentLevel, OWNER_LEVEL_COUNT - 1));
            List<String> levelLore = new ArrayList<>();
            levelLore.add("Current level: " + currentLevel);
            levelLore.add(" ");
            levelLore.addAll(wrapText(currentEmployer.getLevelText(clampedLevelIndex), 32));
            boolean canUpgrade = currentLevel + 1 < WORKER_LEVEL_COSTS.length;
            if (canUpgrade) {
                int nextLevel = currentLevel + 1;
                int upgradeCost = WORKER_LEVEL_COSTS[nextLevel];
                levelLore.add(" ");
                levelLore.addAll(xpStatusLines.apply(upgradeCost));
                levelLore.add("Next level cost: " + upgradeCost + " XP");
                levelLore.add("Click to upgrade");
            } else {
                levelLore.add(" ");
                levelLore.add("Your XP: " + currentExperience);
                levelLore.add("You reached  the maximum level.");
            }
            inventory.setItem(WORK_LEVEL_BUTTON_SLOT, createSimpleItem(
                    Material.ENCHANTED_BOOK,
                    "Worker level " + currentLevel,
                    levelLore
            ));

            int rewardAmount = currentEmployer.salaryBlock().getMaxStackSize();
            List<String> salaryLore = new ArrayList<>();
            salaryLore.add("Cost: " + SALARY_XP_COST + " XP");
            salaryLore.add("Reward: " + rewardAmount + "x " + currentEmployer.salaryBlock().name());
            salaryLore.addAll(xpStatusLines.apply(SALARY_XP_COST));
            salaryLore.add("Click to collect salary");
            inventory.setItem(WORK_SALARY_BUTTON_SLOT, createSimpleItem(
                    currentEmployer.salaryBlock(),
                    "Collect salary",
                    salaryLore
            ));
        } else {
            inventory.setItem(WORK_LEVEL_BUTTON_SLOT, createSimpleItem(
                    Material.GRAY_STAINED_GLASS_PANE,
                    "No level available",
                    List.of("Get a job to unlock upgrades")
            ));
            inventory.setItem(WORK_SALARY_BUTTON_SLOT, createSimpleItem(
                    Material.GRAY_STAINED_GLASS_PANE,
                    "No salary available",
                    List.of("Get a job to collect salary")
            ));
        }

        inventory.setItem(PREV_BUTTON_SLOT, 0 < page
                ? createSimpleItem(Material.ARROW, "Previous Page", List.of("View earlier jobs"))
                : createSimpleItem(Material.GRAY_STAINED_GLASS_PANE, "No previous page", List.of("You are on the first page"))
        );

        inventory.setItem(NEXT_BUTTON_SLOT, page < maxPage && !owners.isEmpty()
                ? createSimpleItem(Material.ARROW, "Next Page", List.of("View more jobs"))
                : createSimpleItem(Material.GRAY_STAINED_GLASS_PANE, "No next page", List.of("You are on the last page"))
        );

        inventory.setItem(CLOSE_BUTTON_SLOT, createSimpleItem(
                Material.BARRIER,
                "Close Menu",
                List.of("Return to gameplay")
        ));

        inventory.setItem(PAGE_INFO_SLOT, createSimpleItem(
                Material.MAP,
                "Page " + (page + 1) + " of " + (maxPage + 1),
                List.of(owners.isEmpty()
                        ? "Nothing to display"
                        : "Showing " + (startIndex + 1) + "-" + endIndex + " of " + owners.size())
        ));
        viewer.openInventory(inventory);
    }

    private record WorkMenuContext(String workerName, int page) {
    }

    private record AdminMenuContext(int page) {
    }

    private record OwnerWorkersContext(String ownerName, boolean adminView, int page) {
    }

    private record WorkMenuData(WorkerState workerState, List<OwnerState> owners, OwnerState currentEmployer) {
    }

    private enum MenuType {
        WORK,
        ADMIN,
        OWNER,
        OWNER_WORKERS,
        OWNER_SALARY,
        UNKNOWN
    }

    private record MenuParseResult(MenuType type,
                                   WorkMenuContext workContext,
                                   AdminMenuContext adminContext,
                                   OwnerMenuContext ownerContext,
                                   OwnerWorkersContext ownerWorkersContext) {
    }

    private static final MenuParseResult UNKNOWN_MENU = new MenuParseResult(MenuType.UNKNOWN, null, null, null, null);

    private void openAdminMenu(Player player, int requestedPage) {
        runAsync(database::loadOwners, owners -> renderAdminMenu(player, owners, requestedPage), error -> {
            log.error("Failed to open admin menu for {}", player.getName(), error);
        });
    }

    private void renderAdminMenu(Player player, List<OwnerState> owners, int requestedPage) {
        int maxPage = owners.isEmpty() ? 0 : (owners.size() - 1) / JOBS_PER_PAGE;
        int page = Math.max(0, Math.min(requestedPage, maxPage));
        int startIndex = page * JOBS_PER_PAGE;
        int endIndex = Math.min(startIndex + JOBS_PER_PAGE, owners.size());

        Inventory inventory = Bukkit.createInventory(
                null,
                ADMIN_MENU_SIZE,
                Component.text(String.format(Locale.ROOT, ADMIN_MENU_TITLE_TEMPLATE, page + 1))
        );

        int slot = 0;
        for (int index = startIndex; index < endIndex; index++, slot++) {
            OwnerState owner = owners.get(index);
            ItemStack item = new ItemStack(Material.PAPER);
            ItemMeta meta = item.getItemMeta();
            if (meta != null) {
                meta.displayName(Component.text("Mine owner: " + owner.ownerName()));
                List<Component> lore = new ArrayList<>();
                lore.add(Component.text("Reward item: " + owner.salaryBlock().name()));
                lore.add(Component.text("XP per block: " + owner.xpPerBlock()));
                lore.add(Component.text("Click to manage this mine"));
                meta.lore(lore);
                meta.getPersistentDataContainer().set(workJobKey, PersistentDataType.STRING, owner.ownerName());
                item.setItemMeta(meta);
            }
            inventory.setItem(slot, item);
        }

        if (owners.isEmpty()) {
            inventory.setItem(ADMIN_MENU_SIZE / 2, createSimpleItem(
                    Material.BOOK,
                    "No mines available",
                    List.of("Owners have not created mines yet.")
            ));
        }

        inventory.setItem(WORK_LEVEL_BUTTON_SLOT, createSimpleItem(
                Material.WRITABLE_BOOK,
                "Administration tips",
                List.of("Click a mine entry", "to open its owner menu")
        ));

        inventory.setItem(WORK_SALARY_BUTTON_SLOT, createSimpleItem(
                Material.COMPASS,
                "Tracked mines: " + owners.size(),
                List.of("Manage owner settings", "without switching accounts")
        ));

        inventory.setItem(PREV_BUTTON_SLOT, 0 < page
                ? createSimpleItem(Material.ARROW, "Previous Page", List.of("View earlier mines"))
                : createSimpleItem(Material.GRAY_STAINED_GLASS_PANE, "No previous page", List.of("You are on the first page"))
        );

        inventory.setItem(NEXT_BUTTON_SLOT, page < maxPage && !owners.isEmpty()
                ? createSimpleItem(Material.ARROW, "Next Page", List.of("View more mines"))
                : createSimpleItem(Material.GRAY_STAINED_GLASS_PANE, "No next page", List.of("You are on the last page"))
        );

        inventory.setItem(CLOSE_BUTTON_SLOT, createSimpleItem(
                Material.BARRIER,
                "Close menu",
                List.of("Return to gameplay")
        ));

        inventory.setItem(PAGE_INFO_SLOT, createSimpleItem(
                Material.MAP,
                "Page " + (page + 1) + " of " + (maxPage + 1),
                List.of(owners.isEmpty()
                        ? "Nothing to display"
                        : "Showing " + (startIndex + 1) + "-" + endIndex + " of " + owners.size())
        ));
        player.openInventory(inventory);
    }

    private ItemStack createSimpleItem(Material material, String displayName, List<String> lore) {
        ItemStack item = new ItemStack(material);
        ItemMeta meta = item.getItemMeta();
        if (meta != null) {
            meta.displayName(Component.text(displayName));
            if (lore != null && !lore.isEmpty()) {
                List<Component> loreComponents = new ArrayList<>(lore.size());
                for (String lineText : lore) {
                    loreComponents.add(Component.text(lineText));
                }
                meta.lore(loreComponents);
            } else {
                meta.lore(null);
            }
            item.setItemMeta(meta);
        }
        return item;
    }

    private int getMaxWorkPage() {
        int owners = database.ownerCount();
        return owners == 0 ? 0 : (owners - 1) / JOBS_PER_PAGE;
    }

    @Override
    public ChunkGenerator getDefaultWorldGenerator(@NotNull String worldName, String id) {
        return new SpaceChunkGenerator();
    }

    @Override
    public void onEnable() {
        if (!getDataFolder().exists()) {
            getDataFolder().mkdirs();
        }

        database = new Database();

        try {
            workerPanelGenerator = new WorkerPanelGenerator(new File(getDataFolder(), "PlotState.yml"));
            ownerMineGenerator = new OwnerMineGenerator(new File(getDataFolder(), "MineState.yml"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        Bukkit.getPluginManager().registerEvents(this, this);
        workJobKey = new NamespacedKey(this, "work_job_id");
    }

    @Override
    public void onDisable() {
        if (database != null) {
            if (!database.connectionPool.isDisposed()) {
                database.connectionPool.disposeLater().block();
            }
        }
    }

    private static RegionManager GetRegionManager() {
        return Objects.requireNonNull(WorldGuard.getInstance()
                .getPlatform()
                .getRegionContainer()
                .get(BukkitAdapter.adapt(Objects.requireNonNull(Bukkit.getWorld("world")))));
    }

    private <T> void runAsync(Supplier<T> task, Consumer<T> onSuccess, Consumer<Throwable> onError) {
        Mono.fromCallable(task::get)
                .subscribeOn(Schedulers.boundedElastic())
                .subscribe(result -> Bukkit.getScheduler().runTask(this, () -> onSuccess.accept(result)),
                        error -> Bukkit.getScheduler().runTask(this, () -> onError.accept(error)));
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull Command command, @NotNull String label, @NotNull String @NotNull [] args) {
        if (!(sender instanceof Player player)) {
            sender.sendMessage("Only players can use this command.");
            return true;
        }
        try {
            String commandName = command.getName();
            if ("work".equalsIgnoreCase(commandName)) {
                if (player.isOp()) {
                    openAdminMenu(player, 0);
                } else {
                    if (database.ownerExists(player.getName())) {
                        openOwnerMenu(player, player.getName(), false);
                    } else {
                        openWorkMenu(player, player.getName(), 0);
                    }
                }
                return true;
            }

            if ("home".equalsIgnoreCase(commandName)) {
                Location home = Objects.requireNonNull(player.getRespawnLocation());
                home.getWorld().getChunkAt(home).load();
                player.teleport(home);
                player.sendMessage("Returned to your home.");
                return true;
            }
        } catch (Exception exception) {
            log.error("Failed onCommand {}", player.getName(), exception);
        }
        return false;
    }

    @EventHandler
    public void onInventoryClick(InventoryClickEvent event) {
        if (!(event.getWhoClicked() instanceof Player player)) {
            return;
        }
        try {
            String title = PLAIN_SERIALIZER.serialize(event.getView().title());
            MenuParseResult menu = UNKNOWN_MENU;

            Matcher workMatcher = WORK_MENU_TITLE_PATTERN.matcher(title);
            if (workMatcher.matches()) {
                String workerName = workMatcher.group("worker").trim();
                String pageGroup = workMatcher.group("page");
                if (!workerName.isEmpty() && pageGroup != null) {
                    try {
                        int displayPage = Integer.parseInt(pageGroup);
                        int requestedPage = Math.max(0, displayPage - 1);
                        menu = new MenuParseResult(
                                MenuType.WORK,
                                new WorkMenuContext(workerName, requestedPage),
                                null,
                                null,
                                null
                        );
                    } catch (NumberFormatException ignored) {
                    }
                }
            } else {
                Matcher adminMatcher = ADMIN_MENU_TITLE_PATTERN.matcher(title);
                if (adminMatcher.matches()) {
                    String pageGroup = adminMatcher.group("page");
                    if (pageGroup != null) {
                        try {
                            int displayPage = Integer.parseInt(pageGroup);
                            int requestedPage = Math.max(0, displayPage - 1);
                            menu = new MenuParseResult(
                                    MenuType.ADMIN,
                                    null,
                                    new AdminMenuContext(requestedPage),
                                    null,
                                    null
                            );
                        } catch (NumberFormatException ignored) {
                        }
                    }
                } else {
                    Matcher ownerMenuMatcher = OWNER_MENU_TITLE_PATTERN.matcher(title);
                    if (ownerMenuMatcher.matches()) {
                        String ownerName = ownerMenuMatcher.group("owner").trim();
                        if (!ownerName.isEmpty()) {
                            menu = new MenuParseResult(
                                    MenuType.OWNER,
                                    null,
                                    null,
                                    new OwnerMenuContext(ownerName, ownerMenuMatcher.group("admin") != null),
                                    null
                            );
                        }
                    } else {
                        Matcher ownerSalaryMatcher = OWNER_SALARY_TITLE_PATTERN.matcher(title);
                        if (ownerSalaryMatcher.matches()) {
                            String ownerName = ownerSalaryMatcher.group("owner").trim();
                            if (!ownerName.isEmpty()) {
                                menu = new MenuParseResult(
                                        MenuType.OWNER_SALARY,
                                        null,
                                        null,
                                        new OwnerMenuContext(ownerName, ownerSalaryMatcher.group("admin") != null),
                                        null
                                );
                            }
                        } else {
                            Matcher ownerWorkersMatcher = OWNER_WORKERS_TITLE_PATTERN.matcher(title);
                            if (ownerWorkersMatcher.matches()) {
                                String ownerName = ownerWorkersMatcher.group("owner").trim();
                                String pageGroup = ownerWorkersMatcher.group("page");
                                if (!ownerName.isEmpty() && pageGroup != null) {
                                    try {
                                        int displayPage = Integer.parseInt(pageGroup);
                                        int requestedPage = Math.max(0, displayPage - 1);
                                        menu = new MenuParseResult(
                                                MenuType.OWNER_WORKERS,
                                                null,
                                                null,
                                                null,
                                                new OwnerWorkersContext(ownerName, ownerWorkersMatcher.group("admin") != null, requestedPage)
                                        );
                                    } catch (NumberFormatException ignored) {
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (menu.type() == MenuType.UNKNOWN) {
                return;
            }

            Inventory topInventory = event.getView().getTopInventory();
            event.setCancelled(true);

            if (event.getRawSlot() >= topInventory.getSize()) {
                return;
            }

            if (menu.type() == MenuType.WORK) {
                WorkMenuContext context = menu.workContext();
                WorkerState workerState = database.findWorker(context.workerName());
                int maxPage = getMaxWorkPage();
                int currentPage = Math.max(0, Math.min(context.page(), maxPage));

                if (event.getRawSlot() == CLOSE_BUTTON_SLOT) {
                    player.closeInventory();
                    return;
                }
                if (event.getRawSlot() == PAGE_INFO_SLOT) {
                    return;
                }
                if (event.getRawSlot() == PREV_BUTTON_SLOT) {
                    if (currentPage > 0) {
                        openWorkMenu(player, context.workerName(), currentPage - 1);
                    }
                    return;
                }
                if (event.getRawSlot() == NEXT_BUTTON_SLOT) {
                    if (currentPage < maxPage) {
                        openWorkMenu(player, context.workerName(), currentPage + 1);
                    }
                    return;
                }
                if (event.getRawSlot() == WORK_SALARY_BUTTON_SLOT || event.getRawSlot() == WORK_LEVEL_BUTTON_SLOT) {
                    String employerName = workerState.employerName();
                    if (employerName == null) {
                        player.sendMessage("You are not currently employed.");
                        return;
                    }
                    OwnerState ownerState = database.findOwner(employerName);
                    if (event.getRawSlot() == WORK_SALARY_BUTTON_SLOT) {
                        int totalExperience = player.getTotalExperience();
                        if (totalExperience < SALARY_XP_COST) {
                            player.sendMessage("You need at least " + SALARY_XP_COST + " XP to collect your salary.");
                        } else {
                            player.giveExp(-SALARY_XP_COST);
                            int updatedExperience = player.getTotalExperience();
                            runAsync(() -> {
                                database.updateWorkerExperience(workerState.workerName(), updatedExperience);
                                return Boolean.TRUE;
                            }, ignored -> {
                            }, error -> log.error("Failed to persist salary experience for {}", workerState.workerName(), error));

                            int rewardAmount = ownerState.salaryBlock().getMaxStackSize();
                            Map<Integer, ItemStack> leftovers = player
                                    .getInventory()
                                    .addItem(new ItemStack(ownerState.salaryBlock(), rewardAmount));
                            if (!leftovers.isEmpty()) {
                                leftovers.values().forEach(item ->
                                        player.getWorld().dropItemNaturally(player.getLocation(), item)
                                );
                            }
                            player.sendMessage("Salary collected: " + rewardAmount + "x " + ownerState.salaryBlock().name() + ".");
                            openWorkMenu(player, context.workerName(), currentPage);
                        }
                    } else {
                        int currentLevel = workerState.level(employerName);
                        if (currentLevel + 1 >= WORKER_LEVEL_COSTS.length) {
                            player.sendMessage("You have already reached the maximum worker level.");
                        } else {
                            int nextLevel = currentLevel + 1;
                            int upgradeCost = WORKER_LEVEL_COSTS[nextLevel];
                            if (player.getTotalExperience() < upgradeCost) {
                                player.sendMessage("You need at least " + upgradeCost + " XP to reach level " + nextLevel + ".");
                            } else {
                                player.giveExp(-upgradeCost);
                                int updatedExperience = player.getTotalExperience();
                                UUID playerId = player.getUniqueId();
                                runAsync(() -> {
                                    Mono.usingWhen(
                                            Mono.from(database.connectionFactory.create()),
                                            (Function<Connection, Mono<Void>>) connection -> Flux.from(connection.createStatement("""
                                                                    UPDATE worker_employments
                                                                    SET level = $1
                                                                    WHERE worker_name = $2 AND is_active = TRUE
                                                            """)
                                                    .bind(0, nextLevel)
                                                    .bind(1, workerState.workerName())
                                                    .execute())
                                            .flatMap(Result::getRowsUpdated)
                                            .then(),
                                            connection1 -> Mono.from(connection1.close())
                                    ).block();
                                    database.updateWorkerExperience(workerState.workerName(), updatedExperience);
                                    return Boolean.TRUE;
                                }, ignored -> {
                                    Player targetPlayer = Bukkit.getPlayer(playerId);
                                    if (targetPlayer == null) {
                                        return;
                                    }
                                    targetPlayer.sendMessage("Your worker level is now " + nextLevel + ".");
                                    String levelMessage = ownerState.getLevelText(Math.min(nextLevel, OWNER_LEVEL_COUNT - 1));
                                    if (levelMessage != null && !levelMessage.isBlank()) {
                                        targetPlayer.sendMessage(levelMessage);
                                    }
                                    openWorkMenu(targetPlayer, context.workerName(), currentPage);
                                }, error -> {
                                    log.error("Failed to upgrade worker level for {}", workerState.workerName(), error);
                                });
                            }
                        }
                    }
                    return;
                }

                ItemStack clicked = event.getCurrentItem();
                if (clicked == null || clicked.getType() == Material.AIR) {
                    return;
                }
                ItemMeta meta = clicked.getItemMeta();
                if (meta == null) {
                    return;
                }
                String rawId = meta.getPersistentDataContainer().get(workJobKey, PersistentDataType.STRING);
                if (rawId == null) {
                    return;
                }
                OwnerState newEmployer = database.findOwner(rawId);
                Location destination = newEmployer.spawnLocation();
                String currentEmployerName = workerState.employerName();
                if (newEmployer.ownerName().equals(currentEmployerName)) {
                    destination.getWorld().getChunkAt(destination).load();
                    player.closeInventory();
                    player.teleport(destination);
                    player.sendMessage("Teleported to " + newEmployer.ownerName() + "'s mine.");
                } else {
                    RegionManager regionManager = GetRegionManager();
                    final boolean hadCurrentEmployer = currentEmployerName != null;
                    final int previousExperience = player.getTotalExperience();
                    if (hadCurrentEmployer) {
                        Objects.requireNonNull(regionManager.getRegion(currentEmployerName + "_ore")).getMembers().removePlayer(player.getUniqueId());
                        player.sendMessage("You left the job at " + currentEmployerName + ".");
                    }
                    String workerName = workerState.workerName();
                    Objects.requireNonNull(regionManager.getRegion(newEmployer.oreRegionId())).getMembers().addPlayer(player.getUniqueId());
                    regionManager.save();
                    destination.getWorld().getChunkAt(destination).load();
                    player.closeInventory();
                    player.teleport(destination);
                    player.sendMessage("You are now working for " + newEmployer.ownerName() + ".");
                    UUID playerId = player.getUniqueId();
                    runAsync(() -> {
                        if (hadCurrentEmployer) {
                            database.updateWorkerExperience(workerName, previousExperience);
                        }
                        Mono.usingWhen(
                                Mono.from(database.connectionFactory.create()),
                                (Function<Connection, Mono<Void>>) connection -> Flux.from(connection.createStatement("""
                                                        UPDATE worker_employments
                                                        SET is_active = FALSE
                                                        WHERE worker_name = $1
                                                """)
                                                .bind(0, workerName)
                                                .execute())
                                        .flatMap(Result::getRowsUpdated)
                                        .thenMany(Flux.from(connection.createStatement("""
                                                                INSERT INTO worker_employments (worker_name, employer_name, experience, level, is_active)
                                                                VALUES ($1, $2, 0, 0, TRUE)
                                                                ON CONFLICT (worker_name, employer_name)
                                                                DO UPDATE SET is_active = TRUE
                                                        """)
                                                .bind(0, workerName)
                                                .bind(1, newEmployer.ownerName())
                                                .execute())
                                        .flatMap(Result::getRowsUpdated)
                                )
                                        .then(),
                                connection1 -> Mono.from(connection1.close())
                        ).block();
                        return database.findWorker(workerName);
                    }, refreshedState -> {
                        Player targetPlayer = Bukkit.getPlayer(playerId);
                        if (targetPlayer == null) {
                            return;
                        }
                        targetPlayer.setTotalExperience(0);
                        targetPlayer.setLevel(0);
                        targetPlayer.setExp(0f);
                        targetPlayer.giveExp(refreshedState.experience(newEmployer.ownerName()));
                    }, error -> {
                        log.error("Failed to update employment for {}", workerName, error);
                        Player targetPlayer = Bukkit.getPlayer(playerId);
                        if (targetPlayer != null) {
                            targetPlayer.sendMessage("Could not update your job. Please try again later.");
                        }
                    });
                }
            } else if (menu.type() == MenuType.ADMIN) {
                AdminMenuContext adminContext = menu.adminContext();
                int ownerCount = database.ownerCount();
                int maxPage = ownerCount == 0 ? 0 : (ownerCount - 1) / JOBS_PER_PAGE;
                int currentPage = Math.max(0, Math.min(adminContext.page(), maxPage));

                if (event.getRawSlot() == CLOSE_BUTTON_SLOT) {
                    player.closeInventory();
                } else if (event.getRawSlot() == PAGE_INFO_SLOT) {
                    return;
                } else if (event.getRawSlot() == PREV_BUTTON_SLOT) {
                    if (currentPage > 0) {
                        openAdminMenu(player, currentPage - 1);
                    }
                } else if (event.getRawSlot() == NEXT_BUTTON_SLOT) {
                    if (currentPage < maxPage) {
                        openAdminMenu(player, currentPage + 1);
                    }
                } else if (event.getRawSlot() == WORK_LEVEL_BUTTON_SLOT || event.getRawSlot() == WORK_SALARY_BUTTON_SLOT) {
                    return;
                } else {
                    ItemStack clicked = event.getCurrentItem();
                    if (clicked == null || clicked.getType() == Material.AIR) {
                        return;
                    }
                    ItemMeta meta = clicked.getItemMeta();
                    if (meta == null) {
                        return;
                    }
                    String rawId = meta.getPersistentDataContainer().get(workJobKey, PersistentDataType.STRING);
                    if (rawId == null) {
                        return;
                    }
                    openOwnerMenu(player, rawId, true);
                }
            } else if (menu.type() == MenuType.OWNER) {
                OwnerMenuContext menuState = menu.ownerContext();
                OwnerState state = database.findOwner(menuState.ownerName());
                boolean adminViewFlag = menuState.adminView();

                if (event.getRawSlot() == OWNER_SALARY_BLOCK_SLOT) {
                    Inventory inventory = Bukkit.createInventory(
                            null,
                            BLOCK_SELECTION_MENU_SIZE,
                            Component.text(formatOwnerContextTitle(OWNER_SALARY_BLOCK_TITLE_TEMPLATE, state.ownerName(), menuState.adminView()))
                    );
                    for (int index = 0; index < ownerSalaryBlockOptions.size() && index < BLOCK_SELECTION_MENU_SIZE - 1; index++) {
                        Material option = ownerSalaryBlockOptions.get(index);
                        inventory.setItem(index, createSimpleItem(option, option.name(), List.of("Click to select this option")));
                    }
                    inventory.setItem(BLOCK_SELECTION_BACK_SLOT, createSimpleItem(Material.ARROW, "Back", List.of("Return to mine management")));
                    player.openInventory(inventory);
                } else if (event.getRawSlot() == OWNER_WORKERS_BUTTON_SLOT) {
                    openOwnerWorkersMenu(player, state.ownerName(), adminViewFlag, 0);
                } else if (event.getRawSlot() == OWNER_XP_SLOT) {
                    int delta = 0;
                    if (event.isLeftClick()) {
                        delta = event.isShiftClick() ? 10 : 1;
                    } else if (event.isRightClick()) {
                        delta = event.isShiftClick() ? -10 : -1;
                    }
                    if (delta != 0) {
                        int newValue = Math.max(1, Math.min(1000, state.xpPerBlock() + delta));
                        String ownerName = state.ownerName();
                        runAsync(() -> {
                            Mono.usingWhen(
                                    Mono.from(database.connectionFactory.create()),
                                    (Function<Connection, Mono<Void>>) connection -> Flux.from(connection.createStatement("""
                                                            UPDATE mine_owners
                                                            SET xp_per_block = $1
                                                            WHERE owner_name = $2
                                                    """)
                                            .bind(0, newValue)
                                            .bind(1, ownerName)
                                            .execute())
                                    .flatMap(Result::getRowsUpdated)
                                    .then(),
                                    connection1 -> Mono.from(connection1.close())
                            ).block();
                            return Boolean.TRUE;
                        }, ignored -> {
                            player.sendMessage("XP per block set to " + newValue + ".");
                            openOwnerMenu(player, ownerName, adminViewFlag);
                        }, error -> {
                            log.error("Failed to adjust XP per block for {}", ownerName, error);
                        });
                    }
                } else if (event.getRawSlot() >= OWNER_LEVEL_START_SLOT && event.getRawSlot() < OWNER_LEVEL_START_SLOT + OWNER_LEVEL_COUNT) {
                    int level = event.getRawSlot() - OWNER_LEVEL_START_SLOT;
                    player.closeInventory();
                    String ownerName = state.ownerName();
                    new ConversationFactory(this)
                            .withModality(true)
                            .withLocalEcho(false)
                            .withTimeout(60)
                            .withFirstPrompt(new StringPrompt() {
                                @Override
                                public @NotNull String getPromptText(@NotNull ConversationContext context) {
                                    return "Enter new message for miner level " + level + " or type cancel to abort:";
                                }

                                @Override
                                public Prompt acceptInput(@NotNull ConversationContext ctx, String input) {
                                    try {
                                        if (input == null) {
                                            Bukkit.getScheduler().runTask(GTARP.this, () -> openOwnerMenu(player, ownerName, adminViewFlag));
                                            return Prompt.END_OF_CONVERSATION;
                                        }
                                        String trimmed = input.trim();
                                        if (trimmed.equalsIgnoreCase("cancel")) {
                                            player.sendMessage("Level text edit cancelled.");
                                            Bukkit.getScheduler().runTask(GTARP.this, () -> openOwnerMenu(player, ownerName, adminViewFlag));
                                        } else {
                                            runAsync(() -> {
                                                Mono.usingWhen(
                                                        Mono.from(database.connectionFactory.create()),
                                                        (Function<Connection, Mono<Void>>) connection -> Flux.from(connection.createStatement("UPDATE mine_owners SET " + Database.LEVEL_COLUMNS[level] + " = $1 WHERE owner_name = $2")
                                                                .bind(0, trimmed)
                                                                .bind(1, ownerName)
                                                                .execute())
                                                        .flatMap(Result::getRowsUpdated)
                                                        .then(),
                                                        connection1 -> Mono.from(connection1.close())
                                                ).block();
                                                return Boolean.TRUE;
                                            }, ignored -> {
                                                player.sendMessage("Updated level " + level + " message.");
                                                openOwnerMenu(player, ownerName, adminViewFlag);
                                            }, error -> {
                                                log.error("Failed to update level text for {}", ownerName, error);
                                                openOwnerMenu(player, ownerName, adminViewFlag);
                                            });
                                        }
                                    } catch (Exception exception) {
                                        log.error("Failed acceptInput {}", player.getName(), exception);
                                        Bukkit.getScheduler().runTask(GTARP.this, () -> openOwnerMenu(player, ownerName, adminViewFlag));
                                    }
                                    return Prompt.END_OF_CONVERSATION;
                                }
                            })
                            .buildConversation(player)
                            .begin();
                } else if (event.getRawSlot() == OWNER_CLOSE_SLOT) {
                    if (menuState.adminView()) {
                        openAdminMenu(player, 0);
                    } else {
                        player.closeInventory();
                    }
                }
            } else if (menu.type() == MenuType.OWNER_WORKERS) {
                OwnerWorkersContext context = menu.ownerWorkersContext();
                List<OwnerWorkerEntry> workers = database.loadOwnerWorkers(context.ownerName());
                int maxPage = workers.isEmpty() ? 0 : (workers.size() - 1) / OWNER_WORKERS_PER_PAGE;
                int currentPage = Math.max(0, Math.min(context.page(), maxPage));

                if (event.getRawSlot() == OWNER_WORKERS_BACK_SLOT) {
                    openOwnerMenu(player, context.ownerName(), context.adminView());
                    return;
                }
                if (event.getRawSlot() == OWNER_WORKERS_PAGE_SLOT) {
                    return;
                }
                if (event.getRawSlot() == OWNER_WORKERS_PREV_SLOT) {
                    if (currentPage > 0) {
                        openOwnerWorkersMenu(player, context.ownerName(), context.adminView(), currentPage - 1);
                    }
                    return;
                }
                if (event.getRawSlot() == OWNER_WORKERS_NEXT_SLOT) {
                    if (currentPage < maxPage) {
                        openOwnerWorkersMenu(player, context.ownerName(), context.adminView(), currentPage + 1);
                    }
                    return;
                }

                ItemStack clicked = event.getCurrentItem();
                if (clicked == null || clicked.getType() == Material.AIR) {
                    return;
                }
                ItemMeta meta = clicked.getItemMeta();
                if (meta == null) {
                    return;
                }
                String workerName = meta.getPersistentDataContainer().get(workJobKey, PersistentDataType.STRING);
                if (workerName == null || workerName.isBlank()) {
                    return;
                }

                runAsync(() -> Mono.usingWhen(
                        Mono.from(database.connectionFactory.create()),
                        (Function<Connection, Mono<WorkerBonusResult>>) connection -> Flux.from(connection.createStatement("""
                                                UPDATE worker_employments
                                                SET experience = experience + $2
                                                WHERE worker_name = $1 AND is_active = TRUE
                                                RETURNING experience, is_active
                                        """)
                                        .bind(0, workerName)
                                        .bind(1, OWNER_WORKER_BONUS)
                                        .execute())
                                .flatMap(result -> result.map((row, metadata) -> new WorkerBonusResult(
                                        Optional.ofNullable(row.get("experience", Integer.class)).orElse(OWNER_WORKER_BONUS),
                                        Boolean.TRUE.equals(row.get("is_active", Boolean.class))
                                )))
                                .single(),
                        connection1 -> Mono.from(connection1.close())
                ).block(), result -> {
                    player.sendMessage("Granted " + OWNER_WORKER_BONUS + " XP bonus to " + workerName + ".");
                    Player targetPlayer = Bukkit.getPlayerExact(workerName);
                    if (targetPlayer != null) {
                        targetPlayer.giveExp(OWNER_WORKER_BONUS);
                        targetPlayer.sendMessage("You received a " + OWNER_WORKER_BONUS + " XP bonus from " + context.ownerName() + ".");
                    }
                    openOwnerWorkersMenu(player, context.ownerName(), context.adminView(), currentPage);
                }, error -> {
                    log.error("Failed to grant bonus from {} to {}", context.ownerName(), workerName, error);
                });
            } else if (menu.type() == MenuType.OWNER_SALARY) {
                OwnerMenuContext context = menu.ownerContext();
                OwnerState state = database.findOwner(context.ownerName());

                if (event.getRawSlot() == BLOCK_SELECTION_BACK_SLOT) {
                    renderOwnerMenu(player, state, context.adminView());
                } else {
                    ItemStack clicked = event.getCurrentItem();
                    if (clicked == null || clicked.getType() == Material.AIR) {
                        return;
                    }
                    Material selected = clicked.getType();
                    String ownerName = state.ownerName();
                    boolean adminViewFlag = context.adminView();
                    runAsync(() -> {
                        Mono.usingWhen(
                                Mono.from(database.connectionFactory.create()),
                                (Function<Connection, Mono<Void>>) connection -> Flux.from(connection.createStatement("""
                                                        UPDATE mine_owners
                                                        SET salary_block = $1
                                                        WHERE owner_name = $2
                                                """)
                                        .bind(0, selected.name())
                                        .bind(1, ownerName)
                                        .execute())
                                .flatMap(Result::getRowsUpdated)
                                .then(),
                                connection1 -> Mono.from(connection1.close())
                        ).block();
                        return Boolean.TRUE;
                    }, ignored -> {
                        player.sendMessage("Payout item set to " + selected.name() + ".");
                        openOwnerMenu(player, ownerName, adminViewFlag);
                    }, error -> {
                        log.error("Failed to update salary block for {}", ownerName, error);
                    });
                }
            }
        } catch (Exception exception) {
            log.error("Failed onInventoryClick {}", player.getName(), exception);
        }
    }

    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockBreak(BlockBreakEvent event) {
        Player player = event.getPlayer();
        try {
            if (!database.ownerExists(player.getName())) {
                WorkerState workerState = database.findWorker(player.getName());
                String employerName = workerState.employerName();
                if (employerName != null) {
                    OwnerState ownerState = database.findOwner(employerName);
                    Block block = event.getBlock();
                    ProtectedRegion region = GetRegionManager().getRegion(ownerState.oreRegionId());
                    if (region != null && region.contains(block.getX(), block.getY(), block.getZ())) {
                        event.setDropItems(false);
                        event.setExpToDrop(0);
                        player.giveExp(ownerState.xpPerBlock());
                        database.updateWorkerExperience(workerState.workerName(), player.getTotalExperience());
                    }
                }
            }
        } catch (Exception exception) {
            log.error("Failed onBlockBreak {}", player.getName(), exception);
        }
    }

    @EventHandler
    public void onPlayerJoin(PlayerJoinEvent event) {
        Player player = event.getPlayer();
        World world = player.getWorld();
        String playerName = player.getName();
        try {
//        for (int i = 0; i < 100; ++i) {
//            workerPanelGenerator.generateNextRoom(world);
//            ownerMineGenerator.generateNextMine(world);
//        }
            if (!player.hasPlayedBefore()) {
                RegionManager regionManager = GetRegionManager();
                if (database.ownerExists(playerName)) {
                    OwnerMineGenerator.ModulePlacement placement = ownerMineGenerator.generateNextMine(world);
                    Location spawn = placement.spawnLocation().clone();
                    world.getChunkAt(spawn).load();
                    player.setRespawnLocation(spawn, true);
                    player.teleport(spawn);

                    String shellRegionId = playerName + "_shell";
                    String oreRegionId = playerName + "_ore";
                    String stairsRegionId = playerName + "_stairs";

                    regionManager.removeRegion(shellRegionId);
                    regionManager.removeRegion(oreRegionId);
                    regionManager.removeRegion(stairsRegionId);

                    ProtectedCuboidRegion shellRegion = new ProtectedCuboidRegion(shellRegionId, placement.shellMin(), placement.shellMax());
                    ProtectedCuboidRegion oreRegion = new ProtectedCuboidRegion(oreRegionId, placement.interiorMin(), placement.interiorMax());
                    oreRegion.setPriority(shellRegion.getPriority() + 1);
                    oreRegion.setFlag(Flags.BLOCK_PLACE, StateFlag.State.DENY);

                    ProtectedCuboidRegion spiralStairsRegion = new ProtectedCuboidRegion(stairsRegionId, placement.spiralStairsMin(), placement.spiralStairsMax());
                    spiralStairsRegion.setPriority(oreRegion.getPriority() + 1);

                    regionManager.addRegion(shellRegion);
                    regionManager.addRegion(oreRegion);
                    regionManager.addRegion(spiralStairsRegion);
                    regionManager.save();

                    double x = spawn.getX();
                    double y = spawn.getY();
                    double z = spawn.getZ();
                    Mono.usingWhen(
                            Mono.from(database.connectionFactory.create()),
                            (Function<Connection, Mono<Void>>) connection -> Flux.from(connection.createStatement("""
                                                            UPDATE mine_owners
                                                            SET spawn_x = $1, spawn_y = $2, spawn_z = $3
                                                            WHERE owner_name = $4
                                                    """)
                                            .bind(0, x)
                                            .bind(1, y)
                                            .bind(2, z)
                                            .bind(3, playerName)
                                            .execute())
                                    .flatMap(Result::getRowsUpdated)
                                    .then(),
                            connection1 -> Mono.from(connection1.close())
                    ).block();
                } else {
                    WorkerPanelGenerator.ModulePlacement placement = workerPanelGenerator.generateNextRoom(world);
                    Location spawn = placement.spawnLocation().clone();
                    world.getChunkAt(spawn).load();
                    player.setRespawnLocation(spawn, true);
                    player.teleport(spawn);
                    player.getInventory().addItem(new ItemStack(Material.DIAMOND_PICKAXE));

                    ProtectedCuboidRegion shellRegion = new ProtectedCuboidRegion(placement.regionBaseId() + "_shell", placement.minCorner(), placement.maxCorner());
                    ProtectedCuboidRegion interiorRegion = new ProtectedCuboidRegion(placement.regionBaseId() + "_interior", placement.interiorMin(), placement.interiorMax());
                    interiorRegion.setPriority(shellRegion.getPriority() + 1);
                    DefaultDomain owners = new DefaultDomain();
                    owners.addPlayer(new BukkitPlayer(WorldGuardPlugin.inst(), player));
                    interiorRegion.setOwners(owners);
                    regionManager.addRegion(shellRegion);
                    regionManager.addRegion(interiorRegion);
                    regionManager.save();
                }
            }
        } catch (Exception exception) {
            log.error("Failed onPlayerJoin {}", player.getName(), exception);
        }
    }

    @EventHandler
    public void onPreSpawn(PreCreatureSpawnEvent e) {
        e.setCancelled(true);
    }
}
