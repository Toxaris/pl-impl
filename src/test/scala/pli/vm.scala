package pli

class VMSpec extends Spec {
  "The virtual machine" when "fetching instructions" should
  "fetch one instruction after another" in {
    val vm = new VM(Array(5, 27, 42))
    vm.fetch should be (5)
    vm.fetch should be (27)
    vm.fetch should be (42)
  }

  it should "respect the current codepointer value" in {
    val vm = new VM(Array(5, 27, 42))
    vm.codepointer = 1
    vm.fetch should be (27)
    vm.codepointer = 1
    vm.fetch should be (27)
    vm.codepointer = 0
    vm.fetch should be (5)
  }
}
