package lfsr

import "fmt"

type Cipher struct {
	secretKey []int
}

// New creates a new stream cipher instance.
func New(secretKey []int) (*Cipher, error) {
	if len(secretKey) != 128 {
		return nil, fmt.Errorf("key length must be equal to 128")
	}
	// Return a copy to ensure the original slice isn't modified elsewhere.
	keyCopy := make([]int, len(secretKey))
	copy(keyCopy, secretKey)

	return &Cipher{
		secretKey: keyCopy,
	}, nil
}

func KeyGen() ([]int, error) {
	return keyGen(128, 0.95, 1000)
}

func (cip *Cipher) Crypt(plaintext []int, iv []int) ([]int, error) {
	if len(iv) != 128 {
		return nil, fmt.Errorf("IV length must be equal to 128")
	}

	stateC, err := addVectorsMod2(cip.secretKey[:39], iv[:39])
	if err != nil {
		return nil, fmt.Errorf("failed to initialize C-register state: %w", err)
	}

	stateD, err := addVectorsMod2(cip.secretKey[39:], iv[39:])
	if err != nil {
		return nil, fmt.Errorf("failed to initialize D-register state: %w", err)
	}

	ciphertext := make([]int, 0, len(plaintext))
	var c, z int

	for _, el := range plaintext {
		c, stateC = clockC(stateC)

		z, stateD, err = clockD(stateD, c)
		if err != nil {
			return nil, err
		}

		// XOR the plaintext bit with the output bit.
		ciphertext = append(ciphertext, (el+z)%2)
	}

	return ciphertext, nil
}

func feedbackC(state []int) int {
	// s[39+t] = s[37+t] + s[25+t] + s[24+t] + s[22+t] + s[8+t] + s[6+t] + s[4+t] + s[t]
	return (state[37] + state[25] + state[24] +
		state[22] + state[8] + state[6] +
		state[4] + state[0]) % 2
}

func fc(state []int) int {
	// c = 2*s[12] + s[20] + 1
	return 2*state[12] + state[20] + 1
}

func feedbackD(state []int) int {
	// u[89+t] = u[88+t] + u[50+t] + u[47+t] + u[36+t] + u[34+t] + u[9+t] + u[6+t] + u[t]
	return (state[88] + state[50] + state[47] +
		state[36] + state[34] + state[9] +
		state[6] + state[0]) % 2
}

func fd(state []int) (int, error) {
	// f_d uses bits at specific indices to form an index into the VECTOR table.
	bits := []int{
		state[80], state[65], state[44], state[30], state[20],
		state[12], state[7], state[3], state[1], state[0],
	}

	index := 0
	for _, b := range bits {
		if b != 0 && b != 1 {
			return 0, fmt.Errorf("state bits must be 0 or 1")
		}
		index = (index << 1) | b
	}

	if index >= len(VECTOR) {
		return 0, fmt.Errorf("calculated index %d is out of bounds for VECTOR", index)
	}
	return VECTOR[index], nil
}

// --- Clocking Functions ---

// clockC advances the C-register state by one step and returns the clocking value 'c' and the new state.
func clockC(state []int) (c int, newState []int) {
	c = fc(state)
	newBit := feedbackC(state)

	// Perform left shift and insert the new bit
	newState = append(state[1:], newBit)
	return c, newState
}

func clockD(state []int, c int) (z int, newState []int, err error) {
	z, err = fd(state)
	if err != nil {
		return 0, nil, err
	}

	newState = state
	for i := 0; i < c; i++ {
		newBit := feedbackD(newState)
		// Perform left shift and insert the new bit
		newState = append(newState[1:], newBit)
	}

	return z, newState, nil
}
