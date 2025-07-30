// Boolean type representations
class T {}
class F {}

func M(_ dt: @escaping (T) -> Void) {
    print("true")
    dt(T())
}
func M(_ df: @escaping (F) -> Void) {
    print("false")
    df(F())
}

// OR function overloads - all combinations except (F,F,F) return T
func Or(_ a1: T, _ a2: T, _ a3: T) -> T { return T() }
func Or(_ a1: T, _ a2: T, _ a3: F) -> T { return T() }
func Or(_ a1: T, _ a2: F, _ a3: T) -> T { return T() }
func Or(_ a1: T, _ a2: F, _ a3: F) -> T { return T() }
func Or(_ a1: F, _ a2: T, _ a3: T) -> T { return T() }
func Or(_ a1: F, _ a2: T, _ a3: F) -> T { return T() }
func Or(_ a1: F, _ a2: F, _ a3: T) -> T { return T() }
func Or(_ a1: F, _ a2: F, _ a3: F) -> F { return F() }

// AND function overloads - only (T,T) returns T
func And(_ a1: T, _ a2: T) -> T { return T() }
func And(_ a1: T, _ a2: F) -> F { return F() }
func And(_ a1: F, _ a2: T) -> F { return F() }
func And(_ a1: F, _ a2: F) -> F { return F() }

// NOT function overloads
func Not(_ a: T) -> F { return F() }
func Not(_ a: F) -> T { return T() }

// Helper function that only accepts T
func MustBeT(_ t: T) {}

// Encode the Boolean predicate: (!x3) & ((!x1) & ((x1 | x2 | x1) & (x2 | x3 | x2)))
M { a in
    M { b in
        M { c in
            MustBeT(
              And(Or(Not(a), Not(b), Not(c)),
                And(Or(a, b, Not(c)),
                  And(Or(a, Not(b), c),
                    And(Or(Not(a), b, c),
                      And(Or(Not(a), Not(b), c),
                        And(Or(a, Not(b), Not(c)),
                             Or(Not(a), b, Not(c))))))))
            )
        }
    }
}
