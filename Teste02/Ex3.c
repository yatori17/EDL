#include <stdio.h>

struct Exp {
    enum operacao{
        Num, Add, Sub, Mul, Div
    }op;
    int result;
    struct Exp *exp1, *exp2;
};
int avalia(struct Exp e){
    switch(e.op){
        case Num:
            return e.result;
            break;
        case Add:
            return avalia (*(e.exp1)) + avalia (*(e.exp2));
            break;
        case Sub:
            return avalia (*(e.exp1)) - avalia (*(e.exp2));
            break;
        case Mul:
            return avalia(*(e.exp1)) * avalia(*(e.exp2));
            break;
        case Div:
            return avalia(*(e.exp1)) / avalia(*(e.exp2));
            break;
        default:
            break;
    }
}

int main(){
    struct Exp a,b,c;
    a.op=Add;
    b.result=0; b.op=Num; c.result= 0; c.op=Num;
    a.exp1=&b; a.exp2=&c;
    avalia(a);
    printf("%d",avalia(a));
}
