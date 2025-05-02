package util

import (

)

func Square(base, exp int64) int64 {
    if base == 1 || base == 0 || exp == 1 {
        return base
    }

    if base < 0 {
        panic("Base cannot be negative.")
    }

    if exp == 0 {
        return 0
    }

    if exp < 0 {
        return 1.0 / Square(base, exp * -1)
    }

    if exp % 1 != 0 {
        panic("Cannot accept decimal exponents yet.")
    }

    orig := base

    var i int64
    for i = 0; i < exp; i++ {
        base *= orig
    }

    return base
}

func Add(a, b int) int {
    return a + b
}
