open CS17setup;

/* Type signature 
horzFlip: (list (list (int))) -> (list (list (int))) */

/* Inputs: a list of lists of ints, mat
   Output: a new list of lists of ints, the elements of mat reversed*/

   type matrix('a) = list(list('a));

   let horzFlip: matrix('a) => matrix('a) = mat =>
   List.rev(mat);
       

   check_expect_list_list_alpha(horzFlip([[1,2,3], [4,5,6],[7,8,9]]), [[7,8,9],[4,5,6],[1,2,3]], "multi element list");
   check_expect_list_list_alpha(horzFlip([[1,2], [3,4]]), [[3,4],[1,2]], "2X2 multi element list");
   check_expect_list_list_alpha(horzFlip([[1], [4],[7]]), [[7],[4],[1]], "1X2 element list");



       