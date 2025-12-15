package util

import (
	"crypto/rand"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"fmt"
)

func HashSHA256(input string) string {
	hash := sha256.Sum256([]byte(input))
	return hex.EncodeToString(hash[:])
}

func RandInt32() (int64, error) {
	var buf [4]byte
	_, err := rand.Read(buf[:])
	if err != nil {
		return 0, fmt.Errorf("failed to generate random bytes: %w", err)
	}

	num := binary.BigEndian.Uint32(buf[:])

	return int64(num), nil
}

func Map[T any, V any](a []T, f func(x T) V) []V {
	res := make([]V, len(a))
	for i, v := range a {
		res[i] = f(v)
	}

	return res
}

func ByteToBit(x byte) int {
	if x == '1' {
		return 1
	} else {
		return 0
	}
}

func BitToByte(x int) byte {
	if x == 1 {
		return '1'
	} else {
		return '0'
	}
}

func StrToBitArray(x string) []int {
	bits := make([]int, 0, len(x)*8)
	for _, c := range []byte(x) {
		for i := 7; i >= 0; i-- {
			bit := (c >> i) & 1
			bits = append(bits, int(bit))
		}
	}
	return bits
}

func BitArrayToStr(bits []int) (string, error) {
	if len(bits)%8 != 0 {
		return "", fmt.Errorf("invalid bit array length %d", len(bits))
	}

	buf := make([]byte, 0, len(bits)/8)

	for i := 0; i < len(bits); i += 8 {
		var b byte
		for j := 0; j < 8; j++ {
			bit := bits[i+j]
			if bit&^1 != 0 {
				return "", fmt.Errorf("invalid bit %d at pos %d", bit, i+j)
			}
			b = b<<1 | byte(bit)
		}
		buf = append(buf, b)
	}

	return string(buf), nil
}
