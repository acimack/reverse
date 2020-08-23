open CS17setup;

let rec reverseHelper: (list('a), list('a)) => list('a) = (alod, part) => switch (alod) {
    | [] => part;
    | [hd, ... tl] => reverseHelper(tl, [hd, ...part]); 
    };
let reverse: list('a) => list('a) = alod => reverseHelper(alod, []);   

check_expect_list_alpha(reverse([]), [], "empty list");