
def main(args: Array[String]): Int32 & Impure = 
    Array.foreach(x -> Console.printLine("Arg: <<${x}>>"), args); 
    let args2 = groupWhenQuoted(ToString.toString('"'), args) |> removeQuoteMarks(ToString.toString('"'));
    Array.foreach(x -> Console.printLine("Arg2: <<${x}>>"), args2); 
    0

def groupWhenQuoted(quoteMark: String, args: Array[String]): Array[String] & Impure = 
    groupWhenQuotedHelper(quoteMark, Array.toList(args), ks -> List.toArray(ks))

///
/// Helper for `groupWhenQuoted`.
///
/// Here we are in the initial state, outside a quotation.
///
def groupWhenQuotedHelper(quoteMark: String, tokens: List[String], k: List[String] ~> Array[String]): Array[String] & Impure = 
    match tokens { 
        case Nil => k(Nil)
        case x :: rest if String.contains(x, quoteMark) => groupWhenQuotedHelperInner(quoteMark, rest, x, ks -> k(ks))
        case x :: rest => groupWhenQuotedHelper(quoteMark, rest, ks -> k(x :: ks))
    }

///
/// Helper for `groupWhenQuoted`.
///
/// Here we are "inside" a quotation, stored so far as `acc`.
///
def groupWhenQuotedHelperInner(quoteMark: String, tokens: List[String], acc: String, k: List[String] ~> Array[String]): Array[String] & Impure = 
    match tokens { 
        case Nil => k(acc :: Nil)
        case x :: rest if String.contains(x, quoteMark) => groupWhenQuotedHelper(quoteMark, rest, ks -> k((acc + " " +  x) :: ks))
        case x :: rest => groupWhenQuotedHelperInner(quoteMark, rest, (acc + " " +  x), k)
    }    

def removeQuoteMarks(quoteMark: String, args: Array[String]): Array[String] & Impure = 
    Array.map(String.replace(quoteMark, ""), args)