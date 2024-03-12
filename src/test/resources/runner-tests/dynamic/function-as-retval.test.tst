import io

def main() = {
    var g1 = generator()
    var g2 = generator()
    print("g1: " + g1())
    print("g1: " + g1())
    print("g2: " + g2())
    print("g1: " + g1())
    print("g1: " + g1())
    print("g2: " + g2())
    print("g2: " + g2())
}

def generator() = {
    var x = 0
    def next() = {
        var ret = x
        x = x + 1
        ret
    }
}

// EXPECTED-OUTPUT
// g1: 0
// g1: 1
// g2: 0
// g1: 2
// g1: 3
// g2: 1
// g2: 2
