import io

def main() = {
    var i = 0
    for i < 5 {
        print(i)
        i = i + 1
    }
}

// EXPECTED-OUTPUT
// 0
// 1
// 2
// 3
// 4
