struct Vec2i {}
struct Vec3i {}
struct Vec2d {}
struct Vec3d {}

func x (_ v: Vec2i) -> Int { 0 }
func x (_ v: Vec3i) -> Int { 0 }
func x (_ v: Vec2d) -> Double { 0.0 }
func x (_ v: Vec3d) -> Double { 0.0 }

func y (_ v: Vec2i) -> Int { 0 }
func y (_ v: Vec3i) -> Int { 0 }
func y (_ v: Vec2d) -> Double { 0 }
func y (_ v: Vec3d) -> Double { 0 }

func z (_ v: Vec3i) -> Int { 0 }
func z (_ v: Vec3d) -> Double { 0 }

func add(_ n: Int, _ m: Int) -> Int { 0 }
func add(_ n: Double, _ m: Double) -> Double { 0 }

func sub(_ n: Int, _ m: Int) -> Int { 0 }
func sub(_ n: Double, _ m: Double) -> Double { 0 }

func sqrt(_ n: Double) -> Double { 0 }

func pow(_ n: Int, _ k: Int) -> Int { 0 }
func pow(_ n: Double, _ k: Int) -> Double { 0 }

func dist(_ p1: Vec3d, _ p2: Vec3d) -> Double { 
  let dx = pow(sub(x(p1), x(p2)), 2)
  let dy = pow(sub(y(p1), y(p2)), 2)
  let dz = pow(sub(z(p1), z(p2)), 2)
  return sqrt(add(dx, add(dy, dz)))
}
