#include <stdio.h>
#include <stdlib.h>


int main(){
    int x =5;
    int y=10;
    int z=y-x;
    for(int i=1;i<=z;i++){
        x+=i;
        printf("%d\n",x);
    }
    printf("%d\n",x);
    return 0;
}