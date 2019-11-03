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

module MemAMTests = AccountManagerTest(AccountManager.AccountManager)

module MemAccountTests = AccountTest(Account.Account)

let tests = List.flatten [
    MemAMTests.tests;
    MemAccountTests.tests;
  ]

let suite = "Order Book Test Suite" >::: tests

let _ = run_test_tt_main suite
