open OUnit2

module type Tests = sig 
  val tests : OUnit2.test list
end

module AccountManagerTest (AM: AccountManager.AccountManager) : Tests = struct 
  open AccountManager
  open Account
  let username1 = "username"
  let password = "password"
  let diff_username = "different username"
  let wrong_pw = "incorrect pw"

  let tests = [
    "Test create account manager" >:: (fun _ -> 
        let m = AccountManager.create () in 
        let accounts = AccountManager.accounts m in 
        assert_equal 0 (List.length accounts));
    "Test register and login new user works" >:: (fun _ -> 
        let m = AccountManager.create () in 
        let a = AccountManager.register m username1 password in 
        let _ = (assert_equal username1 (Account.username a)) in 
        let a_check = AccountManager.login m username1 password in 
        assert_equal username1 (Account.username a_check));
    "Test delete user works" >:: (fun _ -> 
        let m = AccountManager.create () in 
        let _ = AccountManager.register m username1 password in 
        let _ = AccountManager.delete_user m username1 password in 
        ());
  ]
end

module AccountTest (A: Account.Account) : Tests = struct 
  open Account
  let username = "username"
  let ticker = "AAPL"
  let amount = 200.0

  let tests = [
    "Test create, username, and empty balances" >:: (fun _ -> 
        let account = Account.create username in 
        let _ = assert_equal username (Account.username account) in 
        let _ = assert_equal [] (Account.balances account) in 
        ()
      );
    "Test set balances" >:: (fun _ -> 
        let account = Account.create username in 
        let _ = Account.set_balance account ticker amount in 
        let b = Account.balance account ticker in 
        assert_equal amount b
      );
    "Test set balance replaces" >:: (fun _ -> 
        let account = Account.create username in 
        let _ = Account.set_balance account ticker amount in 
        let new_balance = 300.0 in 
        let _ = Account.set_balance account ticker new_balance in 
        assert_equal new_balance (Account.balance account ticker)
      )
  ]
end

module OrderBookTest (OB: OrderBook.OrderBook) : Tests = struct 
  open OrderBook
  let buy1 = {asset = "AAPL"; price = 200.; order_type = Buy; 
              username = "Jill"}
  let sell1 = {asset = "AAPL"; price = 205.; order_type = Sell; 
               username = "Benny"}
  let sell2 = {asset = "AAPL"; price = 206.; order_type = Sell; 
               username = "James"}

  let empty_book = OB.empty
  let insert1 = OB.insert buy1 empty_book
  let insert2 = OB.insert sell1 insert1
  let insert3 = OB.insert sell2 insert2
  let remove1 = OB.remove buy1 insert3

  let test_is_empty name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.is_empty ob))

  let test_size name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.size ob))

  let test_member name o ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.member o ob))

  let tests = [
    test_is_empty "Is empty on empty order book" empty_book true;
    test_is_empty "Is empty on non-empty order book" insert2 false;
    test_size "Size of empty order book" empty_book 0;
    test_size "Size of non empty" insert3 3;
    test_member "No members in empty" buy1 empty_book false;
    test_member "Order is a member" sell1 insert2 true;
    test_member "Order not a member after being removed" buy1 remove1 false;
  ]
end

module MatchingEngineTest (ME: MatchingEngine.MatchingEngine) : Tests = struct 
  open MatchingEngine
  open OrderBook
  let buy1 = {asset = "AAPL"; price = 200.; order_type = Buy; 
              username = "Jill"}
  let sell1 = {asset = "AAPL"; price = 200.; order_type = Sell; 
               username = "Benny"}
  let sell2 = {asset = "AAPL"; price = 206.; order_type = Sell; 
               username = "James"}

  let empty_book = OrderBook.empty
  let insert1 = OrderBook.insert buy1 empty_book
  let insert2 = OrderBook.insert sell1 insert1
  let insert3 = OrderBook.insert sell2 insert2


  (* let printy f =
     match (fst f) with 
     | h :: t -> (fst h).username ^ " " ^ (snd h).username *)

  let test_match_order name ob ord expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (MatchingEngine.matchorder ob ord) 
      )

  let tests = [
    test_match_order "Buy on empty book" empty_book buy1 ([], insert1);
    test_match_order "Transaction Made" insert1 sell1 ([sell1, buy1], empty_book)
  ]
end

module MemAMTests = AccountManagerTest(AccountManager.AccountManager)

module MemAccountTests = AccountTest(Account.Account)

module MemOBTests = OrderBookTest(OrderBook.OrderBook)

module MemMETests = MatchingEngineTest(MatchingEngine.MatchingEngine)

let tests = List.flatten [
    MemAMTests.tests;
    MemAccountTests.tests;
    MemOBTests.tests;
    MemMETests.tests
  ]

let suite = "Order Book Test Suite" >::: tests

let _ = run_test_tt_main suite
