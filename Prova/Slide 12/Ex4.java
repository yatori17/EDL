abstract class Exp{
    abstract int avalia();

    public static class Num extends Exp{
        int valor;
        public Num (int n){
            this.valor= n;
        }
        int avalia(){
            return this.valor;
        }
    }
    public static class Add extends Exp{
        Exp e1, e2;
        Add(Exp a, Exp b){
            this.e1=a;
            this.e2=b;
        }
        int avalia(){
            return e1.avalia() + e2.avalia();
        }
    }
    public static class Sub extends Exp{
        Exp e1,e2;
        Sub(Exp e1,Exp e2){
            this.e1=e1;
            this.e2=e2;
        }
        int avalia(){
            return e1.avalia() - e2.avalia();
        }
    }
}
class Ex4{
    public static void main(String [] args){
        Exp num1= new Exp.Num(1);
        Exp num2= new Exp.Num(2);
        Exp num3 = new Exp.Num(4);
        Exp e0= new Exp.Add(num1,num2);
        Exp e1= new Exp.Sub(e0,num3);
        System.out.println(e0.avalia());
        System.out.println(e1.avalia());

    }
}


