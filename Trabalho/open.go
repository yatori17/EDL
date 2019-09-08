package main
import (  
	"fmt"
	"os"
	"strconv"
)
func fib(n int ) int {
	if n==0 { return 0
	}else if n==1 {return 1
	}else  {return fib(n-1)+ fib(n-2) 
	}
}
func main() {
    f, err := os.Create("text.txt") 
    if err != nil {
		panic("cannot create file")
    }
	defer f.Close()
	for i:=0; i <= 40 ; i++ {
		fmt.Fprintln(f,strconv.Itoa(fib(i)))
	}
}
