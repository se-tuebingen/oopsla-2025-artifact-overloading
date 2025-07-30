func empty(_ d: Double) -> Bool { true }
func empty(_ i: Int) -> Bool { true }
func empty(_ s: String) -> Bool { true }

func add(_ i1: Int, _ i2: Int) -> Int { Int(0) }
func add(_ d1: Double, _ d2: Double) -> Double { Double(0) }
func add(_ s1: String, _ s2: String) -> String { String("") }

func sub(_ i1: Int, _ i2: Int) -> Int { Int(0) }
func sub(_ d1: Double, _ d2: Double) -> Double { Double(0) }

func myShow(_ s: String) -> String { s }

// with anno
func myShow(_ n: Int) -> String {
  if empty(n) {
    return myShow("")
  } else {
    return add("|", myShow(sub(n, Int(1))))
  }
}
