use GetOpt.{ArgDescr, OptDescr};
use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg};
use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder};
    
use GetOpt.{getOpt, usageInfo};

enum Flag {
    case Verbose
    case Version 
    case Input(String)
    case Output(String) 
    case LibDir(String)
}

instance ToString[Flag] {
    def toString(x: Flag): String = match x {
        case Verbose    => "Verbose"
        case Version    => "Version" 
        case Input(s)   => "Input('${s}')"
        case Output(s)  => "Output('${s}')"
        case LibDir(s)  => "LibDir('${s}')"
    }
}

def options(): List[OptDescr[Flag]] = 
        {shortOptions = 'v' :: Nil,         longOptions = "verbose" :: Nil,     argDescriptor = NoArg(Verbose),         explanation = "chatty output on stderr"}
    ::  {shortOptions = 'V' :: '?' :: Nil,  longOptions = "version" :: Nil,     argDescriptor = NoArg(Version),         explanation = "show version number" }
    ::  {shortOptions = 'o':: Nil,          longOptions = "output" :: Nil,      argDescriptor = OptArg(outp, "FILE"),   explanation = "output FILE" }
    ::  {shortOptions = 'c':: Nil,          longOptions = Nil,                  argDescriptor = OptArg(inp, "FILE"),    explanation = "input FILE" }
    ::  {shortOptions = 'L':: Nil,          longOptions = "libdir" :: Nil,      argDescriptor = ReqArg(LibDir,"DIR"),   explanation = "library directory" }
    :: Nil

def inp(x: Option[String]): Flag = match x { 
    case Some(s) => Input(s)
    case None => Input("stdin")
}

def outp(x: Option[String]): Flag = match x { 
    case Some(s) => Output(s)
    case None => Output("stdout")
}


def main(argv: Array[String]): Int32 & Impure = 
    Array.foreach(x -> Console.printLine("Arg: '${x}'"), argv);
    let args = Array.toList(argv);    
    let header = "Usage: <program> [OPTION...] files...";
    let options1 = options();
    match getOpt(Permute, options1, args) {
        case (os, ns, Nil) => { Console.printLine("Optionals: ${os}"); Console.printLine("Non-optionals: ${ns}") }
        case (_,  _, errs) => { Console.printLine("Errors: ${errs}");  Console.printLine(usageInfo(header, options1)) }
    };    
    0