# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Development Commands

ZoomHub uses a custom `./zh` script for all development tasks:

- `./zh init` - Initial setup (installs Haskell Stack, PostgreSQL, VIPS, jq)
- `./zh run` - Build and run frontend + backend with hot reload
- `./zh test` - Run all tests (creates test database, runs migrations, executes
  test suite)
- `./zh test "ModuleName"` - Run specific test module
- `./zh lint` - Lint Haskell code with HLint
- `./zh format` - Format Haskell code with Ormolu
- `./zh format check` - Check formatting without modifying files
- `./zh format install` - Install Ormolu formatter
- `./zh db dump` - Dump development database

## Git conventions

- Do not add all files, i.e. `git add .`. Make sure to only add files relevant
  to the current commit.
- Do not mention Claude AI in commits, link to claude.ai, or any other
  promotional destination.
- Ensure commits have titles no longer than 54 characters and a body that is
  wrapped at or below 72 characters.

## Architecture

ZoomHub is a Haskell web service for creating and serving zoomable images (Deep
Zoom Images). The system has several key components:

### Backend (Haskell)

- **API Server** (`src/ZoomHub/API.hs`) - REST API using Servant framework
- **Web Server** (`src/ZoomHub/Web/Main.hs`) - Web interface and viewer pages
- **Worker** (`src/ZoomHub/Worker.hs`) - Background processing
- **Storage** (`src/ZoomHub/Storage/PostgreSQL/`) - PostgreSQL database layer
  using Squeal
- **Authentication** (`src/ZoomHub/Authentication/`) - OAuth (Kinde), Basic
  auth, sessions
- **AWS Integration** (`src/ZoomHub/AWS/`) - S3 storage and Lambda processing

### Frontend (TypeScript/React)

- **Location**: `frontend/src/`
- **Build**: Snowpack with Tailwind CSS
- **Main Component**: `components/Create.tsx` for file uploads

### Content Processing

- **Lambda Function**: `process-content/` (Node.js with Sharp for image
  processing)
- **Deep Zoom Generation**: Creates DZI (Deep Zoom Images) with tile pyramids
- **Supported Formats**: Images, PDFs, SVGs (extensible architecture)

### Database Schema

- **Migrations**: `src/ZoomHub/Storage/PostgreSQL/Schema/Schema*.hs`
- **Tables**: Content, Users, with proper indexing for performance
- **Database**: PostgreSQL with custom types and Squeal for type-safe queries

### Key Types

- **Content** (`src/ZoomHub/Types/Content.hs`) - Core content entity
- **User** (`src/ZoomHub/Types/User.hs`) - User accounts and authentication
- **DeepZoomImage** (`src/ZoomHub/Types/DeepZoomImage.hs`) - DZI metadata

## Configuration

The application uses environment variables and is configured for AWS deployment:

- Development: `.env` file with dotenvx
- Production: AWS Elastic Beanstalk with environment-specific settings
- Database: PostgreSQL connection via environment variables
- Storage: AWS S3 for source files and generated tiles

## Testing

- **Framework**: Haskell HSpec for unit tests
- **Database**: Separate test database (`zoomhub_test`) created per test run
- **Location**: `tests/` directory with module-specific test files
- **Test Data**: SQL fixtures in `data/` directory

## Build System

- **Haskell**: Stack with LTS resolver (see `stack.yaml`)
- **Frontend**: npm with Snowpack bundler
- **Dependencies**: Defined in `zoomhub.cabal` and `package.json`
- **Executables**: Main server and database migration tool

## Code style

### Haskell

- Avoid 'boolean trap', i.e. positional boolean arguments. Prefer sum types
  instead.
