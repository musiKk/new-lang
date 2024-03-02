import io

def test() = 1

def main() = {
    var v = test()
    print(v)
    print(test())
}

// EXPECTED-OUTPUT
// 1
// 1
