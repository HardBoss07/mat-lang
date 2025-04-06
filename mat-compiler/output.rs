fn main() {
   let mut a = 10;
   if a < 10 {
      println!("a is below 10. the value is {}", a);
      a += 1;
      if a < 8 {
         println!("a is below 8. the value is {}", a);
      }
   }
}
