typedef struct Pair
{
  int a;
  int b;
} Pair;

int main()
{
  Pair p;
  p.a = 10;
  p.b = 20;

  if (p.a < p.b)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}