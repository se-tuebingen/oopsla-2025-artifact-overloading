func add(_ x: Int, _ y: Int) -> Int { return x + y }
func add(_ x: String, _ y: String) -> String { return x + y }
func add(_ x: Double, _ y: Double) -> Double { return x + y }

let address = "127.0.0.1"
let username = "steve"
let password = "1234"
let channel = 11

let url = add("http://",
  add(username,
    add(":",
      add(password,
        add("@",
          add(address,
            add("/api/",
              add(channel,
                "/picture"))))))))

print(url)
