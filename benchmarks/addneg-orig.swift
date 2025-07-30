// https://www.cocoawithlove.com/blog/2016/07/12/type-checker-issues.html

// Int
func add(_ a: Int, _ b: Int) -> Int {
  return a + b
}

// Int8
func add(_ a: Int8, _ b: Int8) -> Int8 {
  return a + b
}

// Int16
func add(_ a: Int16, _ b: Int16) -> Int16 {
  return a + b
}

// Int32
func add(_ a: Int32, _ b: Int32) -> Int32 {
  return a + b
}

// Int64
func add(_ a: Int64, _ b: Int64) -> Int64 {
  return a + b
}

// UInt
func add(_ a: UInt, _ b: UInt) -> UInt {
  return a + b
}

// UInt8
func add(_ a: UInt8, _ b: UInt8) -> UInt8 {
  return a + b
}

// UInt16
func add(_ a: UInt16, _ b: UInt16) -> UInt16 {
  return a + b
}

// UInt32
func add(_ a: UInt32, _ b: UInt32) -> UInt32 {
  return a + b
}

// UInt64
func add(_ a: UInt64, _ b: UInt64) -> UInt64 {
  return a + b
}

// Double
func add(_ a: Double, _ b: Double) -> Double {
  return a + b
}

func negate(_ value: Int) -> Int {
  return -value
}

func negate(_ value: Int8) -> Int8 {
  return -value
}

func negate(_ value: Int16) -> Int16 {
  return -value
}

func negate(_ value: Int32) -> Int32 {
  return -value
}

func negate(_ value: Int64) -> Int64 {
  return -value
}

func negate(_ value: UInt) -> Int {
  return -Int(value)
}

func negate(_ value: UInt8) -> Int8 {
  return -Int8(value)
}

func negate(_ value: UInt16) -> Int16 {
  return -Int16(value)
}

func negate(_ value: UInt32) -> Int32 {
  return -Int32(value)
}

func negate(_ value: UInt64) -> Int64 {
  return -Int64(value)
}

func negate(_ value: Double) -> Double {
  return -value
}

func asType(_ value: Int) -> Int {
  return value
}

func asType(_ value: Int) -> Int8 {
  return Int8(value)
}

func asType(_ value: Int) -> Int16 {
  return Int16(value)
}

func asType(_ value: Int) -> Int32 {
  return Int32(value)
}

func asType(_ value: Int) -> Int64 {
  return Int64(value)
}

func asType(_ value: Int) -> UInt {
  return UInt(value)
}

func asType(_ value: Int) -> UInt8 {
  return UInt8(value)
}

func asType(_ value: Int) -> UInt16 {
  return UInt16(value)
}

func asType(_ value: Int) -> UInt32 {
  return UInt32(value)
}

func asType(_ value: Int) -> UInt64 {
  return UInt64(value)
}

func asType(_ value: Int) -> Double {
  return Double(value)
}

func doubleIdentity(_ d: Double) -> Double {
  return d
}

let result =
  doubleIdentity(
    add(
      negate(add(asType(1), asType(2))),
      add(
        negate(add(asType(3), asType(4))),
        asType(5)
      )
    )
  )
