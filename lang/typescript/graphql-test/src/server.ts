import { ApolloServer } from 'apollo-server';
import { typeDefs } from './schema/typeDefs';
import { postResolvers } from './resolvers/postResolvers';

const server = new ApolloServer({
  typeDefs,
  resolvers: postResolvers,
  csrfPrevention: true,
  cache: 'bounded',
});

server.listen().then(({ url }) => {
  console.log(`🚀 Server ready at ${url}`);
});
