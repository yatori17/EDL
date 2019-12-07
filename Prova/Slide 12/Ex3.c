#include <stdio.h>
#include <stdlib.h>

struct Exp{
    enum operacao{
        Num, Add, Sub
    }op;
    union{
        int valor;
        struct{
            struct Exp *exp1, *exp2;
        };
    };
};
int avalia(struct Exp e){
    switch(e.op){
        case Num:
            return e.valor;
        case Add:
            return avalia (*(e.exp1)) + avalia(*(e.exp2));
        case Sub:
            return avalia (*(e.exp1)) - avalia(*(e.exp2));
        default:
            break;
    }
}

int main(void){

    struct Exp e0,num1, num2,num3,e1;
    e1.op=Sub;
    num3.op=0;
    num3.valor=5;
    num1.op=Num;
    num1.valor = 1;
    num2.op=Num;
    num2.valor =3;
    e0.op=Add;
    e0.exp1=&num1;
    e0.exp2=&num2;
    e1.exp1=&e0;
    e1.exp2= &num3;
    printf("%d\n",avalia(e1));
    return 0;
}