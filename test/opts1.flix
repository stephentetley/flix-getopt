use GetOpt.{ArgDescr, OptionDescr};
use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg};
use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder};
    
use GetOpt.{getOpt, usageInfo};

def main(args: Array[String]): Int32 & Impure = 
    Array.foreach(x -> Console.printLine("Arg: '${x}'"), args);    
    let header = "Usage: <program> [OPTION...] files...";
    let options1 = options();
    match getOpt(Permute, options1, args) {
        case Success(opts)   => {Console.printLine("Optionals: ${opts.options}"); Console.printLine("Non-optionals: ${opts.nonOptions}"); 0}
        case Failure(errs)  => {Console.printLine("Errors: ${errs}");  Console.printLine(usageInfo(header, options1)); 1}
    }
    
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

def options(): List[OptionDescr[Flag]] = 
        {optionIds = 'v' :: Nil,        optionNames = "verbose" :: Nil,   argDescriptor = NoArg(Verbose),         explanation = "chatty output on stderr"}
    ::  {optionIds = 'V' :: '?' :: Nil, optionNames = "version" :: Nil,   argDescriptor = NoArg(Version),         explanation = "show version number" }
    ::  {optionIds = 'o':: Nil,         optionNames = "output" :: Nil,    argDescriptor = OptArg(outp, "FILE"),   explanation = "output FILE" }
    ::  {optionIds = 'c':: Nil,         optionNames = Nil,                argDescriptor = OptArg(inp, "FILE"),    explanation = "input FILE" }
    ::  {optionIds = 'L':: Nil,         optionNames = "libdir" :: Nil,    argDescriptor = ReqArg(LibDir,"DIR"),   explanation = "library directory" }
    :: Nil

def inp(x: Option[String]): Flag = match x { 
    case Some(s) => Input(s)
    case None => Input("stdin")
}

def outp(x: Option[String]): Flag = match x { 
    case Some(s) => Output(s)
    case None => Output("stdout")
}




