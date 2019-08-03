package util

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/md5"
	crand "crypto/rand"
	"encoding/base64"
	"encoding/binary"
	"fmt"
	"io"
	mrand "math/rand"
)

// Is this good enough? Why does Golint complain about not having comments?
const (
	LowercaseSet = "abcdefghijklmnopqrstuvwxyz"
	UppercaseSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	NumbersSet   = "0123456789"
	SymbolsSet   = " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
)

// SetSeedAsString yeet
func SetSeedAsString(in string) {
	h := md5.New()
	io.WriteString(h, in)
	var seed uint64 = binary.BigEndian.Uint64(h.Sum(nil))
	// fmt.Println(seed) // Output MD5-calculated seed
	mrand.Seed(int64(seed))
}

// RandomInt yeet
func RandomInt(min, max int) int {
	return min + mrand.Intn(max-min)
}

// RandomString yeet
func RandomString(length int, set string, s int) string {
	// Check validity of charset selection
	if len(set) != 4 {
		fmt.Println("Charset must have a length of 4.")
		return ""
	}
	for i := 0; i < len(set); i++ {
		if set[i] != '0' && set[i] != '1' {
			fmt.Println("An element in the charset is not a valid binary digit.")
			return ""
		}
	}
	// Return early if "set" parameter is "0000"
	if set == "0000" {
		return ""
	}

	// Check validity of shift value (This is somewhat redundant)
	if s < 0 {
		fmt.Println("Shift value must be non-negative.")
		return ""
	}

	// Create character set
	var charset string
	if set[0] == '1' { // Symbols
		charset += SymbolsSet
	}
	if set[1] == '1' {
		charset += LowercaseSet
	}
	if set[2] == '1' {
		charset += UppercaseSet
	}
	if set[3] == '1' {
		charset += NumbersSet
	}

	// Perform shift
	for i := 0; i < s; i++ {
		mrand.Int()
	}

	// Create random string
	bytes := make([]byte, length)
	for i := 0; i < length; i++ {
		bytes[i] = byte(charset[RandomInt(0, len(charset))])
	}

	// Return
	return string(bytes)
}

// OneLineKeygen yeet
func OneLineKeygen(cs string, l, s int) string {
	// Check validity of sequence length
	if l <= 0 { // Return if sequence length is invalid
		return ""
	}

	// Check validity of shift value
	if s < 0 {
		return ""
	}

	return RandomString(l, cs, s)
}

// StrEncrypt Encrypt string to base64 crypto using AES
func StrEncrypt(key []byte, text string) string {
	// key := []byte(keyText)
	plaintext := []byte(text)

	block, err := aes.NewCipher(key)
	if err != nil {
		panic(err)
	}

	// The IV needs to be unique, but not secure. Therefore it's common to
	// include it at the beginning of the ciphertext.
	ciphertext := make([]byte, aes.BlockSize+len(plaintext))
	iv := ciphertext[:aes.BlockSize]
	if _, err := io.ReadFull(crand.Reader, iv); err != nil {
		panic(err)
	}

	stream := cipher.NewCFBEncrypter(block, iv)
	stream.XORKeyStream(ciphertext[aes.BlockSize:], plaintext)

	// convert to base64
	return base64.URLEncoding.EncodeToString(ciphertext)
}

// StrDecrypt Decrypt from base64 to decrypted string
func StrDecrypt(key []byte, cryptoText string) string {
	ciphertext, _ := base64.URLEncoding.DecodeString(cryptoText)

	block, err := aes.NewCipher(key)
	if err != nil {
		panic(err)
	}

	// The IV needs to be unique, but not secure. Therefore it's common to
	// include it at the beginning of the ciphertext.
	if len(ciphertext) < aes.BlockSize {
		panic("ciphertext too short")
	}
	iv := ciphertext[:aes.BlockSize]
	ciphertext = ciphertext[aes.BlockSize:]

	stream := cipher.NewCFBDecrypter(block, iv)

	// XORKeyStream can work in-place if the two arguments are the same.
	stream.XORKeyStream(ciphertext, ciphertext)

	return fmt.Sprintf("%s", ciphertext)
}
