#ifndef NETWORK_H
#define NETWORK_H

#include <random>
#include <torch/torch.h>

struct networkImpl : torch::nn::Module
{
  torch::nn::Conv2d conv1{nullptr}, conv2{nullptr};
  torch::nn::Linear fc1{nullptr}, fc2{nullptr};

  networkImpl()
  {
    conv1 = register_module("conv1", torch::nn::Conv2d(torch::nn::Conv2dOptions(2, 64, 3).padding(1)));
    conv2 = register_module("conv2", torch::nn::Conv2d(torch::nn::Conv2dOptions(64, 128, 3).padding(1)));
    fc1 = register_module("fc1", torch::nn::Linear(torch::nn::LinearOptions(128 * 8 * 8, 512)));
    fc2 = register_module("fc2", torch::nn::Linear(torch::nn::LinearOptions(512, 64)));

    torch::nn::init::kaiming_uniform_(conv1->weight);
    torch::nn::init::constant_(conv1->bias, 0.0);
    torch::nn::init::kaiming_uniform_(conv2->weight);
    torch::nn::init::constant_(conv2->bias, 0.0);
    torch::nn::init::kaiming_uniform_(fc1->weight);
    torch::nn::init::constant_(fc1->bias, 0.0);
    torch::nn::init::kaiming_uniform_(fc2->weight);
    torch::nn::init::constant_(fc2->bias, 0.0);
  }

  torch::Tensor forward(torch::Tensor x)
  {
    x = torch::relu(conv1(x));
    x = torch::relu(conv2(x));

    x = x.view({x.size(0), -1}); // Flatten

    x = torch::relu(fc1(x));
    x = fc2(x);

    return x;
  }
};

TORCH_MODULE(network);

struct transition
{
  torch::Tensor state;
  uint8_t action;
  float reward;
  torch::Tensor next_state;
  torch::Tensor next_valid_mask;
  bool done;
};

class replay_memory
{
public:
  replay_memory(size_t capacity_) : capacity(capacity_)
  {
    std::random_device rd;
    rng = std::mt19937(rd());
  }

  void push(const transition& t)
  {
    if (memory.size() >= capacity)
    {
      memory.pop_front();
    }
    memory.push_back(t);
  }

  uint64_t size() const
  {
    return memory.size();
  }

  std::vector<transition> sample(size_t batch_size)
  {
    std::vector<transition> batch;
    batch.reserve(batch_size);

    std::uniform_int_distribution<size_t> dist(0, memory.size() - 1);

    for (size_t i = 0; i < batch_size; ++i)
    {
      size_t index = dist(rng);
      batch.push_back(memory[index]);
    }
    return batch;
  }
private:
  std::deque<transition> memory;

  size_t capacity;
  std::mt19937 rng;
};


#endif //NETWORK_H