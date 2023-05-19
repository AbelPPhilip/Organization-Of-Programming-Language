/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0{
        return -1;
    }
    (n * (n+1))/2
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut count = 0;
    for x in ls.iter(){
        if *x >= s && *x <= e{
            count += 1;
        }
    }
    return count;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for x in target.iter(){
        if !set.contains(x){
            return false;
        }
    }
    true
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    let length = ls.len();
    if length == 0 {
        return None;
    }

    let sum = ls.iter().fold(0.0, |sum, &x| sum + x);

    let mean = sum / length as f64; 

    Some(mean)
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut decimal = 0; 
    let mut index = 1;
    for x in ls.iter().rev(){
        if *x==1{
            decimal += index;
        }
        index *= 2;
    }
    return decimal;
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut factors = Vec::new();
    let mut num = n; 

    while num % 2 == 0{
        factors.push(2);
        num /= 2;
    }
    let mut odd_num = 3;

    while num > 1 {
        if num % odd_num == 0{
            factors.push(odd_num);
            num /= odd_num;
        } else{
            odd_num +=2;
        }
    }

    return factors

}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let len = lst.len();
    if len <= 1{
     return lst.to_vec();
    }
 
    let mut rotated = Vec::with_capacity(len);
 
    for i in 1..len{
     rotated.push(lst[i]);
    }
 
    rotated.push(lst[0]);
    rotated
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let len = target.len();
    let strlen = s.len();
    if len > strlen{
        return false;
    }
    for i in 0..=(strlen -len){
        if &s[i..(i+len)] == target {
            return true;
        }
    }
    false
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    let length = s.len();
    if length <= 0 {
        return None;
    }
    let mut start = 0;
    let mut end = 0; 
    let mut max_start = 0; 
    let mut max_end = 0; 
    
    for  i in 1..length{
        if (&s[i..i+1]) == (&s[i-1..i]){
            end = i;
        }else {
            if end - start > max_end - max_start{
                max_start = start; 
                max_end = end; 
            }
            start = i; 
            end = i;
        }
    }

    if end - start > max_end - max_start {
        max_start = start;
        max_end = end;
    }

    Some(&s[max_start..=max_end])
}
