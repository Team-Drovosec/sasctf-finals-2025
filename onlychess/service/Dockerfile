FROM golang:alpine AS builder

WORKDIR /app

COPY ./go.mod /app

RUN go mod download 

COPY . /app

RUN go build -o main ./cmd/main.go

FROM alpine:latest

COPY --from=builder /app/main /main

CMD ["./main"]
