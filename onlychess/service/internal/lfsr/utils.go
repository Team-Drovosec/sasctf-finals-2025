package lfsr

import (
	"crypto/rand"
	"fmt"
	"math"
	"math/big"
)

func shannonEntropy(bits []int) float64 {
	if len(bits) == 0 {
		return 0.0
	}

	counts := make(map[int]int)
	for _, bit := range bits {
		counts[bit]++
	}

	entropy := 0.0
	total := float64(len(bits))
	for _, count := range counts {
		if count > 0 {
			prob := float64(count) / total
			entropy -= prob * math.Log2(prob)
		}
	}
	return entropy
}

func keyGen(length int, entropyThreshold float64, maxAttempts int) ([]int, error) {
	for attempt := 0; attempt < maxAttempts; attempt++ {
		key := make([]int, length)
		sum := 0
		for i := 0; i < length; i++ {
			n, err := rand.Int(rand.Reader, big.NewInt(2))
			if err != nil {
				return nil, fmt.Errorf("failed to generate random bit: %w", err)
			}
			bit := int(n.Int64())
			key[i] = bit
			sum += bit
		}

		if sum == 0 {
			continue // Ensure at least one '1'
		}

		H := shannonEntropy(key)
		if H >= entropyThreshold {
			return key, nil
		}
	}
	return nil, fmt.Errorf("failed to generate key with entropy >= %f in %d attempts", entropyThreshold, maxAttempts)
}

func addVectorsMod2(a, b []int) ([]int, error) {
	if len(a) != len(b) {
		return nil, fmt.Errorf("vectors must be the same size")
	}
	result := make([]int, len(a))
	for i := range a {
		result[i] = (a[i] + b[i]) % 2
	}
	return result, nil
}
