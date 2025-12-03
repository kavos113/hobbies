import * as _ from 'lodash';

interface User {
    name: string;
    age: number;
}

export function processUsers(users: User[]): string{
    return _.map(users, (user: User) => `${user.name} (${user.age})`).sort();
}

const users: User[] = [
    { name: 'Alice', age: 30 },
    { name: 'Bob', age: 25 },
    { name: 'Charlie', age: 35 },
];

const processedNames: string = processUsers(users);
console.log(processedNames); // これはNode.js環境での出力用