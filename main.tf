resource "aws_vpc" "survey-buttler-vpc" {
  cidr_block           = "10.123.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true
  tags = {
    Name = "survey-buttler-vpc"
  }
}

resource "aws_subnet" "survey-buttler-public-subnet" {
  vpc_id                  = aws_vpc.survey-buttler-vpc.id
  cidr_block              = "10.123.1.0/24"
  map_public_ip_on_launch = true
  availability_zone       = "eu-central-1"
  tags = {
    Name = "survey-buttler-public"
  }
}
