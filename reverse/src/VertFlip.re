open CS17setup;

/* Type signature 
vertFlip: (list (list (int))) -> (list (list (int))) */

/* Inputs: a list of lists of ints, mat
   Output: a new list of lists of ints, the individual elements of mat reversed*/

   type matrix('a) = list(list('a));

   /*: matrix('a) => matrix('a) = mat => */

   let vertFlip: matrix('a) => matrix('a) = mat =>
   List.map(List.rev, mat);

       
   check_expect_list_list_alpha(vertFlip([[1,2,3], [4,5,6],[7,8,9]]), [[3,2,1],[6,5,4],[9,8,7]], "multi element list");
   check_expect_list_list_alpha(vertFlip([[1,2],[3,4]]), [[2,1],[4,3]], "2X2 multi element list");
   check_expect_list_list_alpha(vertFlip([[1],[3]]), [[1],[3]], "1X2 multi element list");


       