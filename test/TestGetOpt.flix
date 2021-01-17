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
        case None => Height(1000)
    }

    def options(): List[OptionDescr[Flag]] =
            {optionIds = 'X' :: Nil,        optionNames = "hires" :: Nil,               argDescriptor = NoArg(HiRes),               explanation = "show in high resolution"}
        ::  {optionIds = 'c' :: 'C' :: Nil, optionNames = "colour" :: "color" :: Nil,   argDescriptor = OptArg(colour, "COLOUR"),   explanation = "add filter of COLOUR"}
        ::  {optionIds = 'w':: Nil,         optionNames = "width" :: Nil,               argDescriptor = ReqArg(width, "WIDTH"),     explanation = "image WIDTH" }
        ::  {optionIds = 'h':: Nil,         optionNames = "height" :: Nil,              argDescriptor = ReqArg(height, "HEIGHT"),   explanation = "image HEIGHT" }
        :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Single char options                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCharArgs01(): Bool & Impure =
        let args = ["-X"];
        getOpt(Permute, options(), args) |> Result.isOk

    @test
    def testCharArgs02(): Bool & Impure =
        let args = ["-Xc"];
        getOpt(Permute, options(), args) |> Result.isOk

}