const fs = require('fs');
const { Readable } = require('stream');

const readableStream = fs.createReadStream('my-file.txt', { encoding: 'utf8' });

readableStream.on('data', (chunk) => {
    console.log('Chunk received:', chunk);
});

readableStream.on('end', () => {
    console.log('Finished reading the file.');
});

readableStream.on('error', (err) => {
    console.error('There was an error reading the file:', err);
});