

def fib_aux(n)
    if n == 0 
        return 0 
    elsif n == 1
        return 1
    else 
        return fib_aux(n-1) + fib_aux(n-2)
    end
end

def fib(n)
    sequence = []
    x = 0  
    while x <n
        sequence.push(fib_aux(x))
        x=x+1
    end
    sequence
end

def isPalindrome(n)
    num = n
    reverse = 0
    while n !=0
        rem = n % 10
        reverse = reverse * 10 + rem
        n/=10
    end 

    if num == reverse
        return true
    else 
        return false 
    end     
end

def nthmax(n, a)
    if n > a.length
        return nil
    else
       a.sort()[(a.length-1)-n]
    end
end

def freq(s)
    freqchar = ""
    max = 0
    s.each_char do |char|
        total = s.count(char)
        if total > max
            max = total 
            freqchar = char
        end 
    end 
    freqchar
end


def zipHash(arr1, arr2)
    h = {}
    if arr1.length == arr2.length
        arr1.zip(arr2) do |key, value|
            h[key] = value 
        end 
    else 
        return nil
    end
    h
end 
            


def hashToArray(hash)
    arr = []
    for key in  hash.keys
        arrPair = []
        arrPair.push(key)
        arrPair.push(hash.fetch(key))
        arr.push(arrPair)
    end
    arr
end



def maxProcChain(init, procs)
    max = init
    result = 0
    index = 0
    for func in procs
        result = func.call(init)
        if result > max 
            max = result
        end
        if procs.length > index
            index = index+1
            for i in index..procs.length-1 
                result2 = procs[i].call(result)
                if result2 > max 
                    max = result2
                end
            end 
        end 
    end 
    max
end
