Install third party libraries:

Run the following command to install safepass:
```opam install safepass```

In order to run the code, enter the following command:

```make start```

Make start will compile the main.ml into main.byte and run it, opening up
the text based user interface.

Using the text based user interface:
Enter "login" or "signup" in order to register and use an account.

Once signed in, you can place an order by typing in the prompted format:
ordertype,ticker,amount

Ex.  
Buy,AAPL,10.0  
Sell,AAPL,20.0
