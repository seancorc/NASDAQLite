Install third party libraries:


Run the following commands to install safepass and yojson:
```opam install safepass```  
```opam install yojson```
```opam install cohttp```


In order to run the code, you need to first start the server application with
the command:

```make server```

This will compile the server byte file. Now run the command

```./exchangeServer.byte```

in order to run the server executable. Now you will need to open a new Terminal
window in order to run the client application. As you enter requests on the 
client side, you will also be able to observe that these requests and their 
results are printed on the server application as well.

Now in order to launch the client application, run the following command in 
your new Terminal window to both compile and launch the executable:

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

