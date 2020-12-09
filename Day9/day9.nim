import deques, sequtils, parseutils, strutils, intsets

proc parse(text: string): seq[int] =
  text.splitLines.map(parseInt)

let
  input = parse "input.txt".readFile
  example = parse """35

proc solve(s: seq[int]; l: int): (int, int) =
  var
    d = initDeque[int]()
    p = initIntSet() ## p stands for packedsets, I tried to use it but I do not have it yet
                     ## 
                     ## d and p are synchronized to contain same stuff
    n: int
  for i in 0 ..< l:
    d.addLast(s[i])
    p.incl s[i]
  block findInvalid:
    for i in l ..< s.len:
      n = s[i]
      block findPair:
        for m in d:
          if n != m and n - m in p:
            break findPair
        ## if I get here it means I have not found a sum!
        break findInvalid    ## n is answer for part1
      p.incl n
      p.excl d.popFirst
      d.addLast n
  ## part 2
  var
    nMin, nMax: int
    cumSum = s[0]
  block findSum:
    for k in 0 ..< s.len:
      cumSum = 0
      nMin = int.high
      nMax = 0
      for l in k ..< s.len:
        cumSum += s[l]
        if s[l] < nMin:
          nMin = s[l]
        if s[l] > nMax:
          nMax = s[l]
        if cumSum == n:
          break findSum
        if cumSum > n:
          break
  return (n, nMin + nMax)

echo solve(input, 25)
