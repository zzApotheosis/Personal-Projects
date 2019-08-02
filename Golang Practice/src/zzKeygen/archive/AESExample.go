// Playbook - http://play.golang.org/p/3wFl4lacjX

package main

import (
    "bytes"
    "crypto/aes"
    "crypto/cipher"
    "crypto/md5"
    "crypto/sha256"
    crand "crypto/rand"
    mrand "math/rand"
    "encoding/base64"
    "encoding/hex"
    "errors"
    "fmt"
    "io"
    "strings"
    "os/exec"
    "os"
    "bufio"
    "time"
)

func addBase64Padding(value string) string {
    m := len(value) % 4
    if m != 0 {
        value += strings.Repeat("=", 4 - m)
    }

    return value
}

func removeBase64Padding(value string) string {
    return strings.Replace(value, "=", "", -1)
}

func Pad(src []byte) []byte {
    padding := aes.BlockSize - len(src) % aes.BlockSize
    padtext := bytes.Repeat([]byte{byte(padding)}, padding)
    return append(src, padtext...)
}

func Unpad(src []byte) ([]byte, error) {
    length := len(src)
    unpadding := int(src[length - 1])

    if unpadding > length {
        return nil, errors.New("unpad error. This could happen when incorrect encryption key is used")
    }

    return src[:(length - unpadding)], nil
}

func encrypt(key []byte, text string) (string, error) {
    block, err := aes.NewCipher(key)
    if err != nil {
        return "", err
    }

    msg := Pad([]byte(text))
    ciphertext := make([]byte, aes.BlockSize+len(msg))
    iv := ciphertext[:aes.BlockSize]
    if _, err := io.ReadFull(crand.Reader, iv); err != nil {
        return "", err
    }

    cfb := cipher.NewCFBEncrypter(block, iv)
    cfb.XORKeyStream(ciphertext[aes.BlockSize:], []byte(msg))
    finalMsg := removeBase64Padding(base64.URLEncoding.EncodeToString(ciphertext))
    return finalMsg, nil
}

func decrypt(key []byte, text string) (string, error) {
    block, err := aes.NewCipher(key)
    if err != nil {
        return "", err
    }

    decodedMsg, err := base64.URLEncoding.DecodeString(addBase64Padding(text))
    if err != nil {
        return "", err
    }

    if (len(decodedMsg) % aes.BlockSize) != 0 {
        return "", errors.New("blocksize must be multipe of decoded message length")
    }

    iv := decodedMsg[:aes.BlockSize]
    msg := decodedMsg[aes.BlockSize:]

    cfb := cipher.NewCFBDecrypter(block, iv)
    cfb.XORKeyStream(msg, msg)

    unpadMsg, err := Unpad(msg)
    if err != nil {
        return "", err
    }

    return string(unpadMsg), nil
}

func randomInt(min, max int) int {
    return min + mrand.Intn(max - min)
}

func randomString(len int) string {
    bytes := make([]byte, len)
    for i := 0; i < len; i++ {
        bytes[i] = byte(randomInt(32, 126)) // 32 to 126 for typical ASCII characters
    }
    return string(bytes)
}

func main() {
    // Clear Windows PowerShell
    cmd := exec.Command("cmd", "/c", "cls")
    cmd.Stdout = os.Stdout
    if err := cmd.Run(); err != nil {
        fmt.Println(err)
        return
    }

    mrand.Seed(time.Now().UnixNano())
    mrand.Seed(0)
    fmt.Println(randomString(10))

    data := []byte("Testing here LUL")
    hasher := md5.New()
    hasher.Write(data)
    arr, _ := hex.DecodeString(hex.EncodeToString(hasher.Sum(nil)))
    fmt.Println(arr)

    return // END PROGRAM HERE

    // Create variables
    scanner := bufio.NewScanner(os.Stdin)
    var key []byte = []byte("LKHlhb899Y0zzApotheosis99Y09olUi") // Key must be 128-bit or 256-bit

    // Fetch input string for encryption/decryption
    fmt.Printf("Enter input: ")
    scanner.Scan()
    content := scanner.Text()

    // key := []byte("LKHlhb899Y09olUiLKHlhb899Y09olUi") // Key must be 128-bit or 256-bit
    // key := []byte("LKHlhb899Y09olUi") // Key must be 128-bit or 256-bit
    // key := []byte("Shut yo bitchass up")
    // content := "Hello World TESTING WITH LONGER STRING"

    // Encrypt
    encryptMsg, _ := encrypt(key, content)
    fmt.Println(encryptMsg)

    // Decrypt
    msg, _ := decrypt(key, encryptMsg)
    fmt.Println(msg)
}
