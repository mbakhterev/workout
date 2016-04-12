package main

import (
  "fmt"
//  "os"
)

func decode(s string) int {
  var v int
  n, _ := fmt.Sscan(s, &v)

  if n == 1 {
    return v
  }

  switch s[0] {
  case 'J': return 11
  case 'Q': return 12
  case 'K': return 13
  case 'A': return 14
  }

  return -1
}

func loadDeck() []int {
  var n int
  fmt.Scan(&n)
  D := make([]int, n, n)
  for i := 0; i < n; i += 1 {
    var card string
    fmt.Scan(&card)

    D[i] = decode(card)
  }

	return D
}

func turn (mD []int, nD []int) ([]int, []int, bool) {
  i := 0
  for i < len(mD) && i < len(nD) && mD[i] == nD[i] {
    i += 4
  }

  if i >= len(mD) || i >= len(nD) {
    return mD, nD, true
  }

  T := make([]int, 0, len(mD) + len(nD))

  mT := append(T[:0], mD[i + 1:]...)
  if mD[i] > nD[i] {
    mT = append(append(mT, mD[:i + 1]...), nD[:i + 1]...)
  }

  nT := append(T[len(mT):len(mT)], nD[i + 1:]...)
  if mD[i] < nD[i] {
    nT = append(append(nT, mD[:i + 1]...), nD[:i + 1]...)
  }

  return mT, nT, false
}

func joinDeck(mD []int, nD []int) ([]int, []int) {
  r := make([]int, len(mD) + len(nD))
  copy(r[:len(mD)], mD)
  copy(r[len(mD):], nD)
  return r[:len(mD)], r[len(mD):]
}

func main() {
  nT := 0
  pat := false
  mD, nD := joinDeck(loadDeck(), loadDeck())
  for len(mD) > 0 && len(nD) > 0 && !pat {
    mD, nD, pat = turn(mD, nD)
    nT += 1
  }

  if pat {
    fmt.Println("PAT") // Write answer to stdout
    return
  }

  if len(mD) > 0 {
    fmt.Println(1, nT)
    return
  }

  fmt.Println(2, nT)
}

/*
  fmt.Fprintln(os.Stderr, mD, len(mD))
  fmt.Fprintln(os.Stderr, nD, len(nD))

  test := new([20]int)
  copy(test[0:9], mD[0:9])
  copy(test[10:19], nD[10:19])

  fmt.Fprintln(os.Stderr, test, len(test))

  r := append(test[0:3], 200, 300, 400, 500, 600)

  fmt.Fprintln(os.Stderr, test, len(test))
  fmt.Fprintln(os.Stderr, r, len(r))

  fmt.Fprintln(os.Stderr, r, r[:3], r[3:])
*/
