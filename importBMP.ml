(*

The MIT License (MIT)

Copyright (c) 2014 Nicolas DUBIEN

Permission is hereby granted, free of charge, to any person obtaining a copy of 
this software and associated documentation files (the "Software"), to deal in 
the Software without restriction, including without limitation the rights to use, 
copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the 
Software, and to permit persons to whom the Software is furnished to do so, 
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all 
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR 
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER 
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION 
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.  

*)

(* Example usage:

include "importBMP.ml"

let image_in = importBMP "in.bmp"

*)

open Array

let string_length = String.length
let sub_string = String.sub
let eq_string x y = x = y

let nom_sans_extension nom ext =
  let n = string_length nom
  and n' = string_length ext in
  if n > n' && eq_string ("." ^ ext) (sub_string nom (n-n'-1) (n'+1)) then
    sub_string nom 0 (n-n'-1)
  else
    nom

let int4bytes fichier debut =
    seek_in fichier debut;
    let a = input_byte fichier in
    let b = input_byte fichier in
    let c = input_byte fichier in
    let d = input_byte fichier in
    a + 256 * b + 256 * 256 * c + 256 * 256 * 256 * d

let int3bytes fichier =
    let a = input_byte fichier in
    let b = input_byte fichier in
    let c = input_byte fichier in
    a + 256 * b + 256 * 256 * c

(* let sauteBytes fichier nb =
    for i = 1 to nb do
        let a = input_byte fichier in ();
    done *)

let importBMP fichier_bmp =
    let nom = nom_sans_extension fichier_bmp "bmp" in
    let fichier = open_in  (nom ^ ".bmp") in
    
    (* 1er caractère : seek_in canal_in 0; *)
    (* let taille_fichier = int4bytes fichier 2 in *)
    
    let position_image = int4bytes fichier 10 in
    let w = int4bytes fichier 18 in
    let h = int4bytes fichier 22 in
    let padding = w mod 4 in
    
    print_string "w = ";
    print_int w;
    print_string " , h = ";
    print_int h;
    print_string " , padding = ";
    print_int padding;
    
    seek_in fichier position_image;
    let image = make_matrix h w 0 in
        for i = 0 to h-1 do
            for j = 0 to w-1 do
                    image.(i).(j) <- int3bytes fichier;
            done;
        (* sauteBytes fichier padding; *)
        done;
    image