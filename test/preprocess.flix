
def main(args: Array[String]): Int32 & Impure = 
    Array.foreach(x -> Console.printLine("Arg: <<${x}>>"), args); 
    let dquote = ToString.toString('"');
    let args2 = GetOpt.preprocess({quoteOpen = dquote, quoteClose = dquote, stripQuoteMarks = false}, args);
    Array.foreach(x -> Console.printLine("Arg2: <<${x}>>"), args2); 
    let args3 = GetOpt.preprocess({quoteOpen = dquote, quoteClose = dquote, stripQuoteMarks = true}, args);
    Array.foreach(x -> Console.printLine("Arg3: <<${x}>>"), args3); 
    0
