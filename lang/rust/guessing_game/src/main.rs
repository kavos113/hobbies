use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    println!("Guess the number!");

    let secret = rand::thread_rng().gen_range(1..101);
    println!("secret: {}", secret);

    loop {
        println!("please input your guess.");

        let mut guess = String::new();

        io::stdin()
            .read_line(&mut guess)
            .expect("failed to read line");

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        println!("you guessed: {}", guess);

        match guess.cmp(&secret) {
            Ordering::Less => println!("small"),
            Ordering::Greater => println!("great"),
            Ordering::Equal => {
                println!("correct");
                break;
            },
        }
    }
}
