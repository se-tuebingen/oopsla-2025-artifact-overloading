struct MyInt {}
struct MyString {}

func add(_ n: MyInt) -> MyInt { MyInt() }
func add(_ n: MyString) -> MyInt { MyInt() }

func call<R>(_ f: (MyInt) -> R) -> R { f(MyInt()) }
func call<R>(_ f: (MyString) -> R) -> R { f(MyString()) }

func compose<R>(_ f: @escaping (R) -> R, _ g: @escaping (R) -> R) -> (R) -> R { return f }

print(call(compose(compose(compose(compose(compose(compose(compose(compose(compose(add, add), add), add), add), add), add), add), add), add)))

