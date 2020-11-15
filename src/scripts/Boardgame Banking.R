
rm(list = ls()); # Clear environment.

## Configuration.
player.names <-c("jane", "tanaka");
initial.balance <- 2000;


## Functionality.
history <- function() { print(bank$cash); };

balance <- function() { print(detail$current.balance()); };

pay <- function(name.from, name.to, amount) {
  detail$verify.name(name.from);
  detail$verify.name(name.to);
  x <- detail$current.balance();
  x[name.from] <- x[name.from] - amount;
  x[name.to] <- x[name.to] + amount;
  bank$cash <- rbind(bank$cash, x, make.row.names = FALSE);
  balance();
};

pay.min <- function(name.from, name.to, ...) pay(name.from, name.to, min(...));
pay.max <- function(name.from, name.to, ...) pay(name.from, name.to, max(...));

undo <- function() {
  if (nrow(bank$cash) > 1) bank$cash <- bank$cash[1 : (nrow(bank$cash) - 1), ];
  balance();
};


## Technical stuff.
player.names <- append(player.names, "bank");
bank <- new.env();
bank$cash <- as.data.frame(matrix(initial.balance, nrow = 1, ncol = length(player.names)));
names(bank$cash) <- player.names;
bank$cash[1, "bank"] <- Inf;

detail <- new.env();
detail$current.balance <- function () { return(bank$cash[nrow(bank$cash), ]); };
detail$verify.name <- function(name) {
  if (is.na(match(name, player.names))) stop(paste0("Player \"", name, "\" not found."));
};


## Thank you!
cat # m=^.^=m~~ 
