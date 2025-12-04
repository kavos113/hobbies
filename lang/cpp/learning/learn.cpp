#include "learn.h"

#include <torch/torch.h>
#include <fstream>

#include "network.h"
#include "othello_board.h"

#define NUM_GAMES 1000
#define BATCH_SIZE 32
#define GAMMA 0.99f
#define E_START 0.9f
#define E_END 0.1f
#define TARGET_UPDATE 100

void update_target_network(network& policy_net, network& target_net)
{
  torch::NoGradGuard no_grad;
  auto target_params = target_net->parameters();
  auto policy_params = policy_net->parameters();

  for (size_t i = 0; i < target_params.size(); ++i)
  {
    target_params[i].data().copy_(policy_params[i].data());
  }
}

void learn()
{
  torch::Device device = torch::cuda::is_available() ? torch::kCUDA : torch::kCPU;

  network policy_net;
  network target_net;
  policy_net->to(device);
  target_net->to(device);

  std::cout << "Training started on device: " << device.str() << std::endl;

  // copy
  update_target_network(policy_net, target_net);
  target_net->eval();

  torch::optim::Adam optimizer(policy_net->parameters(), torch::optim::AdamOptions(1e-5));

  replay_memory memory(100000);

  std::cout << "Starting training for " << NUM_GAMES << " games." << std::endl;

  int total_steps = 0;
  for (int game = 0; game < NUM_GAMES; ++game)
  {
    othello_board board;
    float epsilon = E_END + (E_START - E_END) * std::exp(-1.0 * total_steps / 2000);

    torch::Tensor state = board.to_tensor().to(device);

    bool done = false;
    while (!done)
    {
      // select action
      uint8_t action = 0;

      uint64_t valid_puts = board.get_valid_puts();
      std::vector<uint8_t> valid_actions;
      for (uint8_t pos = 0; pos < 64; ++pos)
      {
        if (valid_puts & 1ULL << pos)
          valid_actions.push_back(pos);
      }

      if (valid_actions.empty())
      {
        board.pass_turn();
        state = board.to_tensor().to(device);
        if (board.get_valid_puts() == 0)
          done = true;

        continue;
      }

      // epsilon-greedy
      if (static_cast<float>(rand()) / RAND_MAX < epsilon)
      {
        action = valid_actions[rand() % valid_actions.size()];
      }
      else
      {
        torch::NoGradGuard no_grad;
        torch::Tensor q_values = policy_net->forward(state);

        float max_q = -1e9;
        for (uint8_t pos : valid_actions)
        {
          float q = q_values[0][pos].item<float>();
          if (q > max_q)
          {
            max_q = q;
            action = pos;
          }
        }
      }

      // run action
      othello_board next_board = board;
      bool valid = next_board.put(action);
      if (!valid)
      {
        continue;
      }

      float reward = 0.0f;
      done = next_board.is_game_over();
      if (done)
      {
        reward = static_cast<float>(next_board.stone_diff());
      }

      torch::Tensor next_state = next_board.to_tensor().to(device);

      std::vector next_valid_mask_vec(64, 0.0f);
      uint64_t next_valid_puts = next_board.get_valid_puts();
      if (!done)
      {
        for (uint8_t pos = 0; pos < 64; ++pos)
        {
          if (next_valid_puts & 1ULL << pos)
            next_valid_mask_vec[pos] = 1.0f;
        }
      }
      torch::Tensor next_valid_mask = torch::tensor(next_valid_mask_vec).to(device);

      memory.push({state, action, reward, next_state, next_valid_mask, done});

      // update state
      state = next_state;
      board = next_board;
      total_steps++;

      // learning
      if (memory.size() >= BATCH_SIZE)
      {
        std::vector<transition> batch = memory.sample(BATCH_SIZE);

        std::vector<torch::Tensor> state_batch_vec;
        std::vector<uint8_t> action_batch_vec;
        std::vector<float> reward_batch_vec;
        std::vector<torch::Tensor> next_state_batch_vec;
        std::vector<torch::Tensor> next_valid_mask_batch_vec;
        std::vector<float> done_batch_vec;

        for (const auto& t : batch)
        {
          state_batch_vec.push_back(t.state);
          action_batch_vec.push_back(t.action);
          reward_batch_vec.push_back(t.reward);
          next_state_batch_vec.push_back(t.next_state);
          next_valid_mask_batch_vec.push_back(t.next_valid_mask);
          done_batch_vec.push_back(t.done ? 0.0f : 1.0f);
        }

        torch::Tensor state_batch = torch::cat(state_batch_vec).to(device);
        torch::Tensor action_batch = torch::tensor(action_batch_vec, torch::kLong).to(device);
        torch::Tensor reward_batch = torch::tensor(reward_batch_vec).to(device);
        torch::Tensor next_state_batch = torch::cat(next_state_batch_vec).to(device);
        torch::Tensor next_valid_mask_batch = torch::stack(next_valid_mask_batch_vec).to(device);
        torch::Tensor done_batch = torch::tensor(done_batch_vec).to(device);

        // current Q values
        torch::Tensor current_q_values = policy_net->forward(state_batch)
          .gather(1, action_batch.unsqueeze(1))
          .squeeze(1);

        // target Q values
        torch::Tensor target_q_values;
        {
          torch::NoGradGuard no_grad;
          torch::Tensor next_q_all = target_net->forward(next_state_batch);

          torch::Tensor penarty = torch::full_like(next_q_all, -1e9).to(device);
          torch::Tensor masked_next_q = torch::where(next_valid_mask_batch > 0.5, next_q_all, penarty);

          torch::Tensor next_q_values = std::get<0>(masked_next_q.max(1));
          target_q_values = reward_batch + GAMMA * next_q_values * done_batch;
        }

        torch::Tensor loss = torch::nn::SmoothL1Loss()(current_q_values, target_q_values);

        optimizer.zero_grad();
        loss.backward();

        // torch::nn::utils::clip_grad_norm_(policy_net->parameters(), 1.0);

        optimizer.step();
      }

      if (total_steps % TARGET_UPDATE == 0)
      {
        update_target_network(policy_net, target_net);
      }
    }

    if (game % 10 == 0)
    {
      std::cout << "\rGame: " << game << " completed." << std::flush;
    }
  }

  c10::Dict<std::string, torch::Tensor> model_dict;
  for (const auto& pair : policy_net->named_parameters())
  {
    model_dict.insert(pair.key(), pair.value().cpu());
  }
  std::vector<char> buffer = torch::pickle_save(model_dict);
  std::ofstream ofs("othello_weights.pt", std::ios::binary | std::ios::out);
  ofs.write(buffer.data(), buffer.size());
  ofs.close();

  std::cout << "Training completed and model saved." << std::endl;
}
