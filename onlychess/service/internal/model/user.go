package model

import (
	"fmt"
	"strconv"
	"strings"
)

type User struct {
	Username    string
	AnonymousID int64
}

func ValidateUsername(x string) bool {
	return !strings.Contains(x, "anon:")
}

func Equal(a, b *User) bool {
	if a.AnonymousID != 0 {
		return a.AnonymousID == b.AnonymousID
	}

	return a.Username == b.Username
}

func (u *User) ToString() string {
	if u.AnonymousID != 0 {
		return fmt.Sprintf("anon:%d", u.AnonymousID)
	}

	return u.Username
}

func FromString(x string) (*User, error) {
	if strings.Contains(x, "anon:") {
		anonID, err := strconv.Atoi(x[5:])
		if err != nil {
			return nil, err
		}

		return &User{
			AnonymousID: int64(anonID),
		}, nil
	}

	return &User{Username: x}, nil
}
