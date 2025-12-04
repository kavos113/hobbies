#ifndef OTHELLO_BOARD_H
#define OTHELLO_BOARD_H
#include <cstdint>
#include <iostream>
#include <torch/torch.h>

#define IS_H_MASK 0x8080808080808080ULL
#define IS_A_MASK 0x0101010101010101ULL
#define NOT_H_MASK 0x7F7F7F7F7F7F7F7FULL
#define NOT_A_MASK 0xFEFEFEFEFEFEFEFEULL

#define is_h_col(pos) ((pos) & IS_H_MASK)
#define is_a_col(pos) ((pos) & IS_A_MASK)
#define not_h_col(pos) ((pos) & NOT_H_MASK)
#define not_a_col(pos) ((pos) & NOT_A_MASK)

class othello_board
{
public:
  uint64_t player;
  uint64_t opponent;

  othello_board()
  {
    // first: player is black(X), opponent is white(O)
    player = 0x0000000810000000;
    opponent = 0x0000001008000000;
  }

  othello_board(uint64_t p, uint64_t o) : player(p), opponent(o) {}

  void print_board() const
  {
    std::cout << "-----------------\n  A B C D E F G H" << "\n";
    for (int row = 0; row < 8; ++row)
    {
      std::cout << row + 1 << " ";
      for (int col = 0; col < 8; ++col)
      {
        uint64_t mask = 1ULL << (row * 8 + col);
        if (player & mask)
          std::cout << "X ";
        else if (opponent & mask)
          std::cout << "O ";
        else
          std::cout << ". ";
      }
      std::cout << "\n";
    }
    std::cout << "-----------------\n";
  }

  bool is_game_over() const
  {
    if ((player | opponent) == 0xFFFFFFFFFFFFFFFF)
      return true;

    if (get_valid_puts() != 0)
      return false;

    if (othello_board(opponent, player).get_valid_puts() != 0)
      return false;

    // 両者とも置ける場所がない
    return true;
  }

  // おけなければfalse, 置けたら置いてひっくり返す
  // 左上が0、右下が63
  bool put(uint8_t put_pos)
  {
    // すでに石がある場所には置けない
    if ((player | opponent) & 1ULL << put_pos)
      return false;

    uint64_t put_mask = 1ULL << put_pos;
    uint64_t to_flip = 0;

    // → ← ↓ ↑ ↙ ↖ ↘ ↗
    const int directions[8] = {1, -1, 8, -8, 7, -7, 9, -9};

    // 左右の端でのはみ出し防止

    for (int dir : directions)
    {
      uint64_t current_flip = 0;
      uint64_t runner = put_mask;

      // 各方向に1マスずつ進んでひっくり返せるか確認する
      while (true)
      {
        if (dir == 1 || dir == -7 || dir == 9) // 右方向への移動
        {
          if (is_h_col(runner))
            break;
        }
        else if (dir == -1 || dir == 7 || dir == -9) // 左方向への移動
        {
          if (is_a_col(runner))
            break;
        }

        if (dir > 0)
          runner <<= dir;
        else
          runner >>= -dir;

        if (runner == 0)
          break;

        // 射線上に相手の石がある
        if (opponent & runner)
        {
          current_flip |= runner;
        }
        // 自分の石が見つかる = はさめる
        else if (player & runner)
        {
          to_flip |= current_flip;
          break;
        }
        else
        {
          break;
        }
      }
    }

    if (!to_flip)
      return false;

    player ^= put_mask | to_flip;
    opponent ^= to_flip;

    std::swap(player, opponent);

    return true;
  }

  uint64_t get_valid_puts() const
  {
    uint64_t valid_puts = 0;
    uint64_t empty = ~(player | opponent);

    // →
    // temp: playerの石の右側にあるopponentの石
    // 量が少ないのでdirに対するforループは使わない
    uint64_t temp = opponent & not_a_col(player << 1);
    // 上のと合わせて6回繰り返せばOK
    for (int i = 0; i < 5; ++i) temp |= opponent & not_a_col(temp << 1);
    valid_puts |= empty & not_a_col(temp << 1);

    // ←
    temp = opponent & not_h_col(player >> 1);
    for (int i = 0; i < 5; ++i) temp |= opponent & not_h_col(temp >> 1);
    valid_puts |= empty & not_h_col(temp >> 1);

    // ↓
    temp = opponent & player << 8;
    for (int i = 0; i < 5; ++i) temp |= opponent & (temp << 8);
    valid_puts |= empty & (temp << 8);

    // ↑
    temp = opponent & player >> 8;
    for (int i = 0; i < 5; ++i) temp |= opponent & (temp >> 8);
    valid_puts |= empty & (temp >> 8);

    // ↘
    temp = opponent & not_a_col(player << 9);
    for (int i = 0; i < 5; ++i) temp |= opponent & not_a_col(temp << 9);
    valid_puts |= empty & not_a_col(temp << 9);

    // ↖
    temp = opponent & not_h_col(player >> 9);
    for (int i = 0; i < 5; ++i) temp |= opponent & not_h_col(temp >> 9);
    valid_puts |= empty & not_h_col(temp >> 9);

    // ↙
    temp = opponent & not_h_col(player << 7);
    for (int i = 0; i < 5; ++i) temp |= opponent & not_h_col(temp << 7);
    valid_puts |= empty & not_h_col(temp << 7);

    // ↗
    temp = opponent & not_a_col(player >> 7);
    for (int i = 0; i < 5; ++i) temp |= opponent & not_a_col(temp >> 7);
    valid_puts |= empty & not_a_col(temp >> 7);

    return valid_puts;
  }

  void pass_turn()
  {
    std::swap(player, opponent);
  }

  int stone_diff() const
  {
    return std::popcount(player) - std::popcount(opponent);
  }

  torch::Tensor to_tensor() const
  {
    std::vector board_vec(2 * 8 * 8, 0.0f);

    for (int i = 0; i < 64; ++i)
    {
      if (player & 1ULL << i)
      {
        board_vec[i] = 1.0f;           // player's pieces -> channel 0
      }
      else if (opponent & 1ULL << i)
      {
        board_vec[64 + i] = 1.0f;     // opponent's pieces -> channel 1
      }
    }

    const auto options = torch::TensorOptions().dtype(torch::kFloat32);
    return torch::from_blob(board_vec.data(), {1, 2, 8, 8}, options).clone();
  }
};


#endif //OTHELLO_BOARD_H
