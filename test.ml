open OUnit2

module type Tests = sig 
  val tests : OUnit2.test list
end

module MatchingEngineTest (ME: MatchingEngine.MatchingEngine) = struct 
  open MatchingEngine

  let me1 = ME.load_from_json (Yojson.Basic.from_file "tickers_test.json")
  let me2 = ME.create ()

  let b1 = ("AAPL", 100, 30., 1.)
  let b2 = ("AAPL", 100, 29., 3.)
  let b3 = ("AAPL", 100, 28., 5.)
  let s1 = ("AAPL", 100, 31., 2.)
  let s2 = ("AAPL", 100, 32., 4.)
  let s3 = ("AAPL", 100, 33., 6.)

  let test_member name me string expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (ME.member me string))

  let tests = [
    test_member "yes member" me1 "AAPL" true;
    test_member "no member" me1 "NO" false;
  ]
end

module AccountManagerTest2 (AM: AccountManager.AccountManager) = struct 
  open AccountManager 
  open Account
  open Yojson.Basic.Util

  let empty = AM.create ()

  let manager = AM.create ()
  let manager2 = AM.create ()
  let manager3 = AM.create ()

  let u1 = AM.register manager "Jack" "password"
  let u2 = AM.register manager "Sean" "password"
  let u3 = AM.register manager2 "Jack" "password"

  let a1 = AM.register manager3 "Mark" "password"
  let () = AM.delete_user manager3 "Mark" "password"

  let () = AM.set_account_position manager "Sean" "AAPL" 100
  let () = AM.dec_account_position manager "Sean" "AAPL" 20
  let () = AM.inc_account_position manager "Sean" "AAPL" 5

  let () = AM.set_account_balance manager "Jack" 100.
  let () = AM.dec_account_balance manager "Jack" 10.
  let () = AM.inc_account_balance manager "Jack" 5.

  let () = AM.delete_user manager2 "Jack" "password"
  let f_del_inv_user = fun () -> AM.delete_user manager2 "Fake Name" "password"
  let f_del_inv_pass = fun () -> AM.delete_user manager "Sean" "fake password"

  let f_login_inv_user = fun () -> AM.login manager "Fake Name" "password"
  let f_login_inv_pass = fun () -> AM.login manager "Sean" "fake password"

  let f_register_used_user = fun () -> AM.register manager "Sean" "password2"

  let am2 = AM.load_from_json (Yojson.Basic.from_file "users_test.json")

  let test_accounts name am expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (AM.accounts am))

  let test_balance name account expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (Account.balance account))

  let test_position name account ticker expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (Account.position account ticker))

  let test_error name f expected_error : test = 
    name >:: (fun _ ->
        assert_raises expected_error f)

  let test_login name account name' password expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (AM.login account name' password))

  let tests = [
    test_accounts "empty manager accounts" empty [];
    test_accounts "empty manager accounts created then deleted" manager3 [];
    test_accounts "non-empty manager accounts" manager ["Jack";"Sean"];
    test_accounts "deleted user" manager2 [];
    test_balance "balance" u1 95.;
    test_position "position" u2 "AAPL" 85;
    test_error "delete non user" f_del_inv_user  
      (InvalidUsername "Username does not exist");
    test_error "delete wrong password" f_del_inv_pass 
      InvalidPassword;
    test_error "login non user" f_login_inv_user 
      (InvalidUsername "Username does not exist");
    test_error "login wrong password" f_login_inv_pass 
      InvalidPassword;
    test_login "login user exists" manager "Sean" "password" u2;
    test_error "register used user" f_register_used_user 
      (InvalidUsername "Username is taken")
  ]
end

module AccountTest2 (A: Account.Account) = struct
  open Account
  let empty = A.create_empty "Sean"
  let empty2 = A.create_empty "Aaron"
  let a1 = A.create "Jack" 100.
  let a2 = A.create "Aaron" 100.
  let a3 = A.create "Jimmy" 100.
  let () = A.set_position a2 "AAPL" 100
  let () = A.set_balance a3 0.
  let () = A.set_balance empty2 25.



  let test_username name account expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (A.username account))

  let test_balance name account expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (A.balance account))

  let test_positions name account expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (A.positions account)) 

  let test_position name account ticker expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (A.position account ticker))

  let tests = [
    test_username "Non-empty account name" a1 "Jack";
    test_username "Empty account name" empty "Sean";
    test_balance "Empty account balance" empty 0.;
    test_balance "Non-empty account balance" a1 100.;
    test_balance "Non-empty account balance 2" empty2 25.;
    test_balance "Set account balance to 0." a3 0.;
    test_balance "Non-empty account balance" a2 100.;
    test_positions "Non-empty account no positions" a1 [];
    test_positions "Non-empty account no positions 2" empty2 [];
    test_positions "Empty account no positions" empty [];
    test_positions "Non-empty account one position" a2 [("AAPL",100)];
    test_position "Non-empty account no position" a1 "AAPL" 0;
    test_position "Empty account no position" empty2 "AAPL" 0;
    test_position "Non-empty account AAPL position" a2 "AAPL" 100;
  ]
end

module OrderBookTest2 (OB: OrderBook.OrderBook) = struct 
  open OrderBook
  let b1 = ("AAPL", 100, 30., 1.)
  let b2 = ("AAPL", 100, 29., 3.)
  let b3 = ("AAPL", 100, 28., 5.)
  let s1 = ("AAPL", 100, 31., 2.)
  let s2 = ("AAPL", 100, 32., 4.)
  let s3 = ("AAPL", 100, 33., 6.)

  let o1 = ("Jack", 1, 30., 60.)
  let o2 = ("Sean", 1, 30., 61.)
  let o3 = ("Aaron", 2, 30., 2.)
  let o4 = ("Aaron", 1, 30., 2.)

  let empty_book = OB.empty

  let ob1 = OB.insert_order empty_book (Buy, o1)
  let ob2 = OB.insert_order ob1 (Sell, o2)

  let ob3 = OB.insert_order ob1 (Sell, o3)
  let ob4 = OB.insert_order empty_book (Sell, o4)

  let ob5 = OB.insert_order empty_book (Sell, o1)
  let ob6 = OB.insert_order ob5 (Buy, o3)
  let ob7 = OB.insert_order empty_book (Buy, o4)

  let make1 = OB.insert_order empty_book (Buy, b1)
  let make2 = OB.insert_order make1 (Buy, b2)
  let make3 = OB.insert_order make2 (Buy, b3)
  let make4 = OB.insert_order make3 (Sell, s1)
  let make5 = OB.insert_order make4 (Sell, s2)
  let book = OB.insert_order make5 (Sell, s3)

  let order1 = ("Jack", 10, 30., 1.)
  let order2 = ("Aaron", 20, 30., 2.)

  let order3 = ("Jack", 10, 30., 1.)
  let order4 = ("Aaron", 20, 30., 1.)

  let orderbook1 = OB.insert_order empty_book (Buy, order1)
  let orderbook2 = OB.insert_order orderbook1 (Buy, order2)

  let orderbook3 = OB.insert_order empty_book (Sell, order1)
  let orderbook4 = OB.insert_order orderbook3 (Sell, order2)

  let orderbook5 = OB.insert_order empty_book (Sell, order3)
  let orderbook6 = OB.insert_order orderbook5 (Sell, order4)

  let orderbook7 = OB.insert_order empty_book (Buy, order3)
  let orderbook8 = OB.insert_order orderbook7 (Buy, order4)

  let test_is_empty name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.is_empty ob))

  let test_no_buys name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.no_buys ob))

  let test_no_sells name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.no_sells ob))

  let test_size name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.size ob))

  let test_best_bid name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.best_bid ob))

  let test_best_offer name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.best_offer ob))

  let test_pop_best_offer name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.pop_best_offer ob))

  let test_pop_best_bid name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.pop_best_bid ob))

  let test_construct_tx name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.construct_tx ob))

  let test_buys name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.buys ob))

  let test_sells name ob expected_output : test = 
    name >:: (fun _ -> 
        assert_equal expected_output (OB.sells ob))

  let tests = [
    test_is_empty "Is empty true" empty_book true;
    test_is_empty "Is empty false" book false;
    test_size "Size of empty book" empty_book 0;
    test_size "Size of non empty" book 6;
    test_no_buys "No buys true" empty_book true;
    test_no_buys "No buys false" book false;
    test_no_sells "No sells true" make3 true;
    test_no_sells "No sells false" book false;
    test_best_bid "No best bid" empty_book None;
    test_best_bid "Yes best bid" book (Some b1);
    test_best_offer "No best offer" make2 None;
    test_best_offer "Yes best offer" book (Some s1);
    test_pop_best_offer "No pop best offer" make3 (None, make3);
    test_pop_best_offer "Yes pop best offer" make4 (Some s1, make3);
    test_pop_best_bid "No pop best bid" empty_book (None, empty_book);
    test_pop_best_bid "Yes pop best bid" make1 (Some b1, empty_book);
    test_construct_tx "Pop1" book (None, book);
    test_construct_tx "Pop2" ob2 (Some (30.,1,"Jack","Sean"), empty_book);
    test_construct_tx "Pop3" ob3 (Some (30.,1,"Jack","Aaron"), ob4);
    test_construct_tx "Pop4" ob6 (Some (30.,1,"Aaron","Jack"), ob7);
    test_construct_tx "Empty" empty_book (None, empty_book);
    test_construct_tx "None" make1 (None, make1);
    test_sells "empty sells" empty_book [];
    test_buys "empty buys" empty_book [];
  ]
end

module MemOBTests = OrderBookTest2(OrderBook.OrderBook)
module MemACCTests = AccountTest2(Account.Account)
module MemACCMANTests = AccountManagerTest2 (AccountManager.AccountManager)
module MemMETests = MatchingEngineTest (MatchingEngine.MatchingEngine)

let tests = List.flatten [
    MemOBTests.tests;
    MemACCTests.tests;
    MemACCMANTests.tests;
    MemMETests.tests;
  ]

let suite = "Order Book Test Suite" >::: tests

let _ = run_test_tt_main suite
