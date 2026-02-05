export default function SomeComponent(props: {
    message: string
}) {
    return (
        <div>
            <h2>{props.message}</h2>    
            <p>This is some component</p>
        </div>
    )
}