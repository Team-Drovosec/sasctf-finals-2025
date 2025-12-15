package lfsr

import (
	"reflect"
	"testing"

	"github.com/c2fo/testify/assert"
)

func Test_EncryptDecrypt(t *testing.T) {
	key, err := keyGen(128, 0.95, 1000)
	assert.NoError(t, err)

	iv, err := keyGen(128, 0.95, 1000)
	assert.NoError(t, err)

	plaintext := []int{1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1}

	cipher, err := New(key)
	assert.NoError(t, err)

	ciphertext, err := cipher.Crypt(plaintext, iv)
	assert.NoError(t, err)

	decipher, err := New(key)
	assert.NoError(t, err)

	decryptedText, err := decipher.Crypt(ciphertext, iv)
	assert.NoError(t, err)

	if !reflect.DeepEqual(plaintext, decryptedText) {
		t.Errorf("Decrypted text does not match original plaintext.\nWant: %v\nGot:  %v", plaintext, decryptedText)
	}
}

func Test_EncryptEqual(t *testing.T) {
	key := []int{1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	iv := []int{0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1}

	plaintext := []int{1, 0, 1, 0, 1, 0, 1, 0}

	cipher, err := New(key)
	if err != nil {
		t.Fatalf("Failed to create new stream cipher for encryption: %v", err)
	}

	ciphertext, err := cipher.Crypt(plaintext, iv)
	if err != nil {
		t.Fatalf("Encryption failed: %v", err)
	}

	if !reflect.DeepEqual(ciphertext, []int{1, 0, 0, 0, 0, 1, 0, 1}) {
		t.Errorf("Decrypted text does not match")
	}
}

func TestNewStreamCipher_InvalidKey(t *testing.T) {
	// Key is too short
	shortKey := make([]int, 127)
	_, err := New(shortKey)
	if err == nil {
		t.Error("Expected an error for a key length of 127, but got nil")
	}

	// Key is too long
	longKey := make([]int, 129)
	_, err = New(longKey)
	if err == nil {
		t.Error("Expected an error for a key length of 129, but got nil")
	}
}
