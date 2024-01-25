package org.fhir.pascal.generator.codegen;

import java.util.List;

import org.hl7.fhir.utilities.Utilities;

/*
Copyright (c) 2011+, HL7, Inc
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this 
list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, 
this list of conditions and the following disclaimer in the documentation 
and/or other materials provided with the distribution.
* Neither the name of HL7 nor the names of its contributors may be used to 
endorse or promote products derived from this software without specific 
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.

*/

public class GeneratorUtils {
	
	public static boolean isDelphiReservedWord(String word) {
	  if (word.equals("and")) return true;
	  if (word.equals("array")) return true;
	  if (word.equals("as")) return true;
	  if (word.equals("asm")) return true;
	  if (word.equals("begin")) return true;
	  if (word.equals("case")) return true;
	  if (word.equals("class")) return true;
	  if (word.equals("const")) return true;
	  if (word.equals("constructor")) return true;
	  if (word.equals("create")) return true;
	  if (word.equals("destructor")) return true;
	  if (word.equals("dispinterface")) return true;
	  if (word.equals("div")) return true;
	  if (word.equals("do")) return true;
	  if (word.equals("downto")) return true;
	  if (word.equals("else")) return true;
	  if (word.equals("end")) return true;
	  if (word.equals("except")) return true;
	  if (word.equals("exports")) return true;
	  if (word.equals("file")) return true;
	  if (word.equals("finalization")) return true;
	  if (word.equals("finally")) return true;
	  if (word.equals("for")) return true;
	  if (word.equals("function")) return true;
	  if (word.equals("goto")) return true;
	  if (word.equals("if")) return true;
	  if (word.equals("implementation")) return true;
	  if (word.equals("in")) return true;
	  if (word.equals("inherited")) return true;
	  if (word.equals("initialization")) return true;
	  if (word.equals("inline")) return true;
	  if (word.equals("interface")) return true;
	  if (word.equals("is")) return true;
	  if (word.equals("label")) return true;
	  if (word.equals("library")) return true;
	  if (word.equals("link")) return true;
	  if (word.equals("mod")) return true;
	  if (word.equals("nil")) return true;
	  if (word.equals("not")) return true;
	  if (word.equals("object")) return true;
	  if (word.equals("of")) return true;
	  if (word.equals("or")) return true;
	  if (word.equals("out")) return true;
	  if (word.equals("packed")) return true;
	  if (word.equals("procedure")) return true;
	  if (word.equals("program")) return true;
	  if (word.equals("property")) return true;
	  if (word.equals("raise")) return true;
	  if (word.equals("record")) return true;
	  if (word.equals("repeat")) return true;
	  if (word.equals("resourcestring")) return true;
	  if (word.equals("set")) return true;
	  if (word.equals("shl")) return true;
	  if (word.equals("shr")) return true;
	  if (word.equals("string")) return true;
	  if (word.equals("then")) return true;
	  if (word.equals("threadvar")) return true;
	  if (word.equals("to")) return true;
	  if (word.equals("try")) return true;
	  if (word.equals("type")) return true;
	  if (word.equals("unit")) return true;
	  if (word.equals("until")) return true;
	  if (word.equals("uses")) return true;
	  if (word.equals("var")) return true;
	  if (word.equals("while")) return true;
	  if (word.equals("with")) return true;
	  if (word.equals("xor")) return true;
    return false;
	}
	
	

}
