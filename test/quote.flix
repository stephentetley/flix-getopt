use GetOpt.{OptionDescr};
use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg};
use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder};
use GetOpt.{getOpt};

/// Experiment to see how we might handle spaces

enum Flag {
    case Input(String)
    case Output(String)
}


instance ToString[Flag] {
    def toString(x: Flag): String = match x {
        case Input(s)   => "Input(<<${s}>>)"
        case Output(s)   => "Output(<<${s}>>)"        
    }
}


def options(): List[OptionDescr[Flag]] =
       {optionIds = 'i':: Nil,     optionNames = "input" :: Nil,   argDescriptor = ReqArg(x -> Some(Input(x)), "INFILE"),     explanation = "source INFILE"}
    :: {optionIds = 'o':: Nil,     optionNames = "output" :: Nil,  argDescriptor = ReqArg(x -> Some(Output(x)), "OUTFILE"),   explanation = "target OUTFILE"}
    :: Nil


def main(args: Array[String]): Int32 & Impure = 
    Array.foreach(x -> Console.printLine("Arg: '${x}'"), args);  
    match getOpt(Permute, options(), args) {
        case Success(opts)   => {Console.printLine("Optionals: ${opts.options}"); Console.printLine("Non-optionals: ${opts.nonOptions}"); 0}
        case Failure(errs)  => {Console.printLine("Errors: ${errs}"); 1}
    }

/// Transcript below. It looks like we will need our own function to preprocess `args` and join quoted arguments.


/// "--input=source_file1.txt --output=dest_file1.txt"
/// Arg: '--input=source_file1.txt'
/// Arg: '--output=dest_file1.txt'
/// Optionals: Input(<<source_file1.txt>>) :: Output(<<dest_file1.txt>>) :: Nil
/// Non-optionals: Nil

/// "--input=source file1.txt --output=dest file1.txt"
/// Arg: '--input=source'
/// Arg: 'file1.txt'
/// Arg: '--output=dest'
/// Arg: 'file1.txt'
/// Optionals: Input(<<source>>) :: Output(<<dest>>) :: Nil
/// Non-optionals: file1.txt :: file1.txt :: Nil

/// "--input='source file1.txt' --output='dest file1.txt'"
/// Arg: '--input='source'
/// Arg: 'file1.txt''
/// Arg: '--output='dest'
/// Arg: 'file1.txt''
/// Optionals: Input(<<'source>>) :: Output(<<'dest>>) :: Nil
/// Non-optionals: file1.txt' :: file1.txt' :: Nil

/// "--input=\"source file1.txt\" --output=\"dest file1.txt\""
/// Arg: '--input="source'
/// Arg: 'file1.txt"'
/// Arg: '--output="dest'
/// Arg: 'file1.txt"'
/// Optionals: Input(<<"source>>) :: Output(<<"dest>>) :: Nil
/// Non-optionals: file1.txt" :: file1.txt" :: Nil


