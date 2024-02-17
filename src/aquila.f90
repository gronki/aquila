module aquila
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, aquila!"
  end subroutine say_hello
end module aquila
