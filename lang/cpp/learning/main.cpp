#include <iostream>

#include "learn.h"
#include "othello_board.h"

int main()
{
  othello_board board;
  board.print_board();

  uint64_t valid_puts = board.get_valid_puts();
  for (uint8_t pos = 0; pos < 64; ++pos)
  {
    if (valid_puts & (1ULL << pos))
      std::cout << "Valid put position: " << static_cast<int>(pos) << std::endl;
  }

  uint8_t put_pos = 37;

  if (board.put(put_pos))
  {
    std::cout << "Placed at position: " << static_cast<int>(put_pos) << std::endl;
    board.print_board();
  }
  else
  {
    std::cout << "Invalid move at position: " << static_cast<int>(put_pos) << std::endl;
  }

  learn();
}
