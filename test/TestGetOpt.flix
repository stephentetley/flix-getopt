/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace TestGetOpt {

    use GetOpt.{OptionDescr};
    use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg};
    use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder};
    use GetOpt.{getOpt};

    /// Preliminary code to define options...

    enum Flag {
        case HiRes
        case Colour(Option[String])
        case Width(Int32)
        case Height(Int32)
    }

    instance Eq[Flag] {
        def eq(x: Flag, y: Flag): Bool = match (x, y) {
            case (HiRes, HiRes)             => true
            case (Colour(x1), Colour(y1))   => x1 == y1
            case (Width(x1), Width(y1))     => x1 == y1
            case (Height(x1), Height(y1))   => x1 == y1
            case (_, _)                     => false
        }

        def neq(x: Flag, y: Flag): Bool = not (x == y)
    }

    instance ToString[Flag] {
        def toString(x: Flag): String = match x {
            case HiRes      => "A"
            case Colour(x)  => "Colour(${x})"
            case Width(i)   => "Width('${i}')"
            case Height(i)  => "Height('${i}')"
        }
    }

    def colour(x: Option[String]): Flag = Colour(x)

    def width(x: String): Flag = match FromString.fromString(x) {
        case Some(i) => Width(i)
        case None => Width(1000)
    }

    def height(x: String): Flag = match FromString.fromString(x) {
        case Some(i) => Height(i)
        case None => Height(760)
    }

    def options(): List[OptionDescr[Flag]] =
            {optionIds = 'X' :: Nil,        optionNames = "hires" :: Nil,               argDescriptor = NoArg(HiRes),               explanation = "show in high resolution"}
        ::  {optionIds = 'c' :: 'C' :: Nil, optionNames = "colour" :: "color" :: Nil,   argDescriptor = OptArg(colour, "COLOUR"),   explanation = "add filter of COLOUR"}
        ::  {optionIds = 'w':: Nil,         optionNames = "width" :: Nil,               argDescriptor = ReqArg(width, "WIDTH"),     explanation = "image WIDTH" }
        ::  {optionIds = 'h':: Nil,         optionNames = "height" :: Nil,              argDescriptor = ReqArg(height, "HEIGHT"),   explanation = "image HEIGHT" }
        :: Nil

    def assertSuccessWith(x: Validation[a, e], test: a -> Bool): Bool  = match x {
        case Success(x1) => test(x1)
        case Failure(_) => false
    }

    def assertFailureWith(x: Validation[a, e], test: Nel[e] -> Bool): Bool  = match x {
        case Success(_) => false
        case Failure(es) => test(es)
    }

    /////////////////////////////////////////////////////////////////////////////
    // No command line arguments                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNoArgs01(): Bool & Impure =
        let args = [];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == Nil

    /////////////////////////////////////////////////////////////////////////////
    // Non-options                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNonOptions01(): Bool & Impure =
        let args = ["non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == "non_option1" :: Nil

    @test
    def testNonOptions02(): Bool & Impure =
        let args = ["non_option1", "non_option2"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == "non_option1" :: "non_option2" :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Single char options                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCharArgs01(): Bool & Impure =
        let args = ["-X"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Nil) and (x.nonOptions == Nil)


    @test
    def testCharArgs02(): Bool & Impure =
        let args = ["-Xc"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs03(): Bool & Impure =
        let args = ["-XcBlue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs04(): Bool & Impure =
        let args = ["-X", "-c"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs05(): Bool & Impure =
        let args = ["-X", "-cBlue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs06(): Bool & Impure =
        let args = ["-X", "-cBlue", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testCharArgs07(): Bool & Impure =
        let args = ["-X", "-cBlue", "-w1024", "-h768"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == Nil)

    @test
    def testCharArgs08(): Bool & Impure =
        let args = ["-X", "-cBlue", "-w1024", "-h768", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testCharArgs09(): Bool & Impure =
        /// "-m" not defined
        let args = ["-m"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testCharArgs10(): Bool & Impure =
        /// "-m" not defined
        let args = ["-Xm"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testCharArgs11(): Bool & Impure =
        /// "-m" not defined
        let args = ["-X", "-m"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1


}