use std::cmp::Ordering;
use std::collections::HashMap;


pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}


struct Node<T> {
    priority: i32,
    data: T,
}


impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}



impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
   
    fn enqueue(&mut self, ele: T) -> () {
        self.push(ele); //Add Element  
        
        let mut index = self.len()-1; 
        while index > 0 {
            let parent_index = (index-1)/2; 
            if self[index] < self[parent_index]{
                self.swap(index, parent_index);
                index = parent_index;
            }else{
                break;
            }
        }
    }

    fn dequeue(&mut self) -> Option<T> {
        if self.is_empty(){
            None
        } else if self.len() == 1{
            self.pop()
        } else {
            let removed = self.remove(0);

            let mut index = 0;
            let mut left_child = 2*index +1;
            let mut right_child = 2*index +2;
            while left_child < self.len(){
                 let mut min_child = left_child;
                 if right_child < self.len() && self[right_child] < self[left_child]{
                    min_child = right_child;
                 }
                 if self[index] < self[min_child]{
                    break;
                 }
                 self.swap(index, min_child);
                 index = min_child;
                 left_child = 2 * index +1; 
                 right_child = 2 * index + 2;
            }
            Some(removed)
        }
    }

    
    fn peek(&self) -> Option<&T> {
        if self.is_empty(){
            None
        }else {
            Some(&self[0])
        }
    }
}

pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let mut x = p2.0 - p1.0;
    let mut y = p2.1 - p1.1;
    if x< 0{
        x *= -1;
    }
    if y < 0{
        y *= -1;
    }
    return x + y;
}


pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) { 
    let mut stark_queue = Vec::new();
    let stark_key = String::from("Stark");
    let stark_coord = allies[&stark_key];

    for (enemy, enemy_coord) in enemies {
        let dist = distance (stark_coord, *enemy_coord);
        stark_queue.enqueue(Node{
            priority: dist, 
            data: enemy.as_str(),
        })
    }
    while let Some(enemy) = stark_queue.dequeue(){
        let mut closer_ally = false; 
        let enemy_data = String::from(enemy.data);
        let enemy_coord = enemies[&enemy_data];
        for (ally,ally_coord) in allies {
            if *ally == &stark_key{
                continue; 
            }
            let dist_to_stark = distance(stark_coord, enemy_coord);
            let dist_to_ally = distance(*ally_coord, enemy_coord);
            if dist_to_ally < dist_to_stark{
                closer_ally = true;
                break; 
            }
        }

        if !closer_ally {
            return (enemy.data, enemies[&enemy_data].0,enemies[&enemy_data].1);
        }
    }
    panic!("Stark is free, no enemy found")
}
