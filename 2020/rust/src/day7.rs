use std::collections::{HashMap, HashSet, VecDeque};

pub struct Rule<'a> {
    fgraph: HashMap<&'a str, HashMap<&'a str, i32>>,
    bgraph: HashMap<&'a str, HashMap<&'a str, i32>>,
}

pub fn partition<'a>(string: &'a str, what: &'a str) -> (&'a str, &'a str, &'a str) {
    if let Some(index) = string.find(what) {
        (
            &string[..index],
            &string[index..index + what.len()],
            &string[index + what.len()..],
        )
    } else {
        (&string[..], &string[0..0], &string[0..0])
    }
}

pub fn rpartition<'a>(string: &'a str, what: &'a str) -> (&'a str, &'a str, &'a str) {
    if let Some(index) = string.rfind(what) {
        (&string[..index - what.len() + 1], what, &string[index..])
    } else {
        (&string[0..0], &string[0..0], &string[..])
    }
}

// enforcing that the string lives same as Rule object or outlives it.
// if u dont want that -> copy -> slow :(
impl<'a> Rule<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut rules = Self {
            fgraph: HashMap::new(),
            bgraph: HashMap::new(),
        };
        for line in source.split("\n") {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            rules.add_rule(&line[..line.len() - 1]);
        }
        rules
    }

    fn parse(edge: &'a str) -> (&'a str, i32) {
        let (count, _, bag) = partition(edge, " ");
        let (bag, _, _) = rpartition(bag, " ");
        (bag, count.parse::<i32>().unwrap())
    }

    fn add_rule(&mut self, rule: &'a str) {
        let (bag, _, edges) = partition(&rule, " bags contain ");
        if edges != "no other bags" {
            for edge in edges.split(", ") {
                let (k, v) = Self::parse(edge);
                self.bgraph
                    .entry(k)
                    .or_insert(HashMap::new())
                    .insert(bag, v);
                self.fgraph
                    .entry(bag)
                    .or_insert(HashMap::new())
                    .insert(k, v);
            }
        } else {
            self.fgraph.insert(bag, HashMap::new());
        }
    }

    pub fn find_containers_for(&self, bag: &str) -> HashSet<String> {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(bag);
        let mut container = HashSet::new();
        while !queue.is_empty() {
            if let Some(c) = queue.pop_front() {
                if visited.contains(&c) {
                    continue;
                }
                if let Some(p) = self.bgraph.get(c) {
                    container.extend(p.keys().map(|&k| String::from(k)));
                    queue.extend(p.keys().map(|&k| k));
                    visited.insert(c);
                }
            }
        }
        container
    }

    pub fn weight(&self, bag: &str) -> i32 {
        self.weight_cache(&mut HashMap::new(), bag)
    }

    fn weight_cache(&self, cache: &mut HashMap<&str, i32>, bag: &str) -> i32 {
        match cache.get(bag) {
            Some(&i) => i,
            None => self.fgraph[bag]
                .iter()
                .map(|(&other, &count)| count * (1 + self.weight_cache(cache, other)))
                .sum(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &'static str = r#"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."#;
    #[test]
    fn part1() {
        let mut set = HashSet::new();
        set.insert(String::from("bright white"));
        set.insert(String::from("muted yellow"));
        set.insert(String::from("dark orange"));
        set.insert(String::from("light red"));
        let other_set = Rule::new(INPUT).find_containers_for("shiny gold");
        assert_eq!(other_set, set);
    }

    #[test]
    fn part2() {
        let rule = Rule::new(INPUT);
        assert_eq!(rule.weight("shiny gold"), 32);
    }
}
