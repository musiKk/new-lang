import io

def main() = {
    var arr = []int(1, 2, 3, 4)
    print(arr[3])
    print(arr[1])

    arr[3] = 0
    print(arr[3])
}

// EXPECTED-OUTPUT
// 4
// 2
// 0
