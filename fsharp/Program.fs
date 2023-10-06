open System
open System.IO
open System.Numerics
open System.Runtime.Intrinsics
open FSharp.NativeInterop
open System.Collections.Generic
open System.Text.Json

#nowarn "9"

let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
    let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
    Span<'a>(p, length)

[<Struct>]
type Post =
    { _id: string
      title: string
      tags: string[] }

type RelatedPosts =
    { _id: string
      tags: string[]
      related: Post[] }

let srcDir = __SOURCE_DIRECTORY__

let posts =
    JsonSerializer.Deserialize<Post[]>(File.ReadAllText $"{srcDir}/../posts.json")

let stopwatch = Diagnostics.Stopwatch()
stopwatch.Start()

// Start work
let tagPostsTmp = Dictionary<string, Stack<int>>()

posts
|> Array.iteri (fun postId post ->

    for tag in post.tags do

        match tagPostsTmp.TryGetValue tag with
        | true, s -> s.Push postId
        | false, _ ->
            let newStack = Stack()
            newStack.Push postId
            tagPostsTmp[tag] <- newStack)

// convert from Dict<_,Stack<int>> to Dict<_,int[]> for faster access
let tagPosts = Dictionary(tagPostsTmp.Count)

for kv in tagPostsTmp do
    tagPosts[kv.Key] <- kv.Value.ToArray()

[<Literal>]
let topN = 5

let shuffles = [|
    Vector256.Create (5, 0, 1, 2, 3, 7, 7, 7)
    Vector256.Create (0, 5, 1, 2, 3, 7, 7, 7)
    Vector256.Create (0, 1, 5, 2, 3, 7, 7, 7)
    Vector256.Create (0, 1, 2, 5, 3, 7, 7, 7)
    Vector256.Create (0, 1, 2, 3, 5, 7, 7, 7)
|]

let allRelatedPosts : RelatedPosts[] =
    posts
    |> Array.mapi (fun postId post ->

        let taggedPostCount = stackalloc posts.Length
        let mutable top5TagCounts = Vector256.Create 0
        let mutable top5PostIds = Vector256.Create 0

        for tagId in post.tags do
            for relatedPostId in tagPosts[tagId] do
                taggedPostCount[relatedPostId] <- taggedPostCount[relatedPostId] + 1uy

        taggedPostCount[postId] <- 0uy // ignore self

        let mutable minTagCount = 0uy

        for relatedPostId in 0 .. taggedPostCount.Length - 1 do
            let relatedPostTagCount = taggedPostCount[relatedPostId]

            if relatedPostTagCount > minTagCount then

                let relatedPostTagCountVec = Vector256.Create (int relatedPostTagCount)

                let comparison = Vector256.GreaterThan (relatedPostTagCountVec, top5TagCounts)
                let moveMask = Vector256.ExtractMostSignificantBits comparison
                let indexOfInsertPoint = (BitOperations.TrailingZeroCount moveMask)
                let shuffle = shuffles[indexOfInsertPoint]

                // Insert new Post
                top5TagCounts <- top5TagCounts.WithElement (5, int relatedPostTagCount)
                top5PostIds <- top5PostIds.WithElement (5, relatedPostId)

                // Shuffle the values down
                top5TagCounts <- Vector256.Shuffle (top5TagCounts, shuffle)
                top5PostIds <- Vector256.Shuffle (top5PostIds, shuffle)

                minTagCount <- byte (top5TagCounts.GetElement 4)


        let top5Posts = Array.zeroCreate topN

        for i in 0..top5Posts.Length - 1 do
            top5Posts[i] <- posts[top5PostIds[i]]

        {
            _id = post._id
            tags = post.tags
            related = top5Posts
        })


stopwatch.Stop()
printfn "Processing time (w/o IO): %dms" stopwatch.ElapsedMilliseconds
let json = JsonSerializer.Serialize allRelatedPosts

File.WriteAllText($"{srcDir}/../related_posts_fsharp.json", json)
