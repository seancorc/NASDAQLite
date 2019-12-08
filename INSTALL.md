Install third party libraries:

Run the following command to install safepass:
```opam install safepass```
```opam install cohttp```

In order to run the code, enter the following command:

```make start```

Make start will compile the main.ml into main.byte and run it, opening up
the text based user interface.

Using the text based user interface:
Enter "login" or "signup" in order to register and use an account.

Once signed in, you can place an order by typing in the prompted format:
ordertype, ticker, order size, price (price is not needed for market orders)

**The available tickers for orders are: AAPL, MSFT, AMZN, ROKU, GOOG

Ex.  

Buy,AAPL,100,10.0  

Sell,ROKU,100,20.0

Buy Market,MSFT,100

Sell Market,AMZN,100

In order for orders to be filled, there must be multiple accounts with orders
from each one. So, create two (or more) accounts and place orders which should
match and fill (they can be partially-filling, market orders, etc.). 

Note: a market order with no opposing orders (buying at market with no sell 
orders in the market) will not fill.

